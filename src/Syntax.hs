module Syntax where
import qualified Data.Set as S

-- For parser

data ProofLine = Seq [Int] LineNumber Formulae RuleReference
    deriving (Show, Eq, Ord)

data RuleReference = AssmptionReference
          | ConjuncRefIntro Int Int
          | ConjuncRefElimi Int
          | ImplicaRefIntro Int (Maybe DischargeRef)
          | ImplicaRefElimi Int DischargeRef
          | RaaRef Int Int (Maybe DischargeRef)
          | NegationRefIntro Int (Maybe DischargeRef)
          | NegationRefElimi
          | DoubleNegationRefElimi Int
          | OrRefElimi Int Int DischargeRef Int DischargeRef
          | OrRefIntro Int
    deriving (Show, Eq, Ord)

type DischargeRef = Int

type ListOfSequents = [ProofLine]

-- For actual proof system

normalize :: Formulae -> Formulae
normalize form = 
    case form of
        Sentence l Conjunction r
            | l <= r -> form
            | otherwise ->  Sentence r Conjunction l
        Sentence l Disjunction r
            | l <= r -> form
            | otherwise ->  Sentence r Disjunction l
        _ -> form

data Proof = Sequent {sequentLineNum :: LineNumber, sequentAssump :: Assumptions, sequentFormulae :: Formulae, sequentRule :: Rule}
    deriving (Eq, Ord)

instance Show Proof where
    show (Sequent ln as for rule) = ppAssump as++" ("++show ln++") "++show for++" "++show rule

type LineNumber = Int

type Assumptions = S.Set Formulae

ppAssump :: S.Set Formulae -> String
ppAssump set = (show (S.toList set))

data Formulae = Sentence Formulae Connective Formulae
              | Atom String
              | Negated Formulae
              | Contradiction

instance Show Formulae where
    show (Atom s) = s
    show (Sentence f1 con f2) = "("++show f1++" "++show con++" "++show f2++")"
    show (Negated f) = "¬"++show f

instance Eq Formulae where
    x == y = 
        case normalize x of
          Sentence l1 Conjunction r1 ->
            case normalize y of
              Sentence l2 Conjunction r2 ->
                normalize l1 == normalize l2 && normalize r1 == normalize r2
              _ -> False
          Sentence l1 Disjunction r1 ->
            case normalize y of
              Sentence l2 Disjunction r2 ->
                normalize l1 == normalize l2 && normalize r1 == normalize r2
              _ -> False
          Sentence l1 Implication r1 ->
            case normalize y of
              Sentence l2 Implication r2 ->
                normalize l1 == normalize l2 && normalize r1 == normalize r2
              _ -> False
          Negated f1 ->
            case normalize y of
              Negated f2->
                normalize f1 == normalize f2
              _ -> False
          Atom f1 ->
            case y of
              Atom f2->
                 f1 == f2
              _ -> False
          Contradiction ->
            case y of
              Contradiction -> True
              _ -> False

instance Ord Formulae where
    x `compare` y = 
        case (normalize x, normalize y) of
          ((Sentence l1 c1 r1), (Sentence l2 c2 r2))
            | c1 /= c2 -> compare c1 c2
            | otherwise -> 
              case compare (normalize l1) (normalize l2) of
                EQ -> compare (normalize r1) (normalize r2)
                ret -> ret
          ((Sentence _ _ _), _) -> GT
          ((Atom a), (Sentence _ _ _)) -> LT
          ((Atom a), (Atom b)) -> compare a b
          ((Atom _), _) -> GT
          (Negated _, Sentence _ _ _) -> LT
          (Negated _, Atom _) -> LT
          (Negated x, Negated y) -> compare (normalize x) (normalize y)
          (Negated _, Contradiction) -> GT
          (Contradiction, Contradiction) -> EQ
          (Contradiction, _) -> LT
              
data Connective = Conjunction
                | Implication
                | Disjunction
    deriving (Eq, Ord)

instance Show Connective where
    show Implication = "➞"
    show Conjunction = "∧"
    show Disjunction = "⋁"

data Rule = AssmptionRule
          | ConjuncRuleIntro Proof Proof
          | ConjuncRuleElimi Proof
          | ImplicaRuleIntro Proof (Maybe Formulae)
          | ImplicaRuleElimi Proof Proof
          | RaaRule Proof Proof (Maybe Formulae)
          | NegationRuleIntro Proof (Maybe Formulae)
          | NegationRuleElimi
          | DoubleNegationRuleElimi Proof
          | OrRuleElimi Proof Proof Formulae Proof Formulae
          | OrRuleIntro Proof
    deriving (Eq, Ord)

instance Show Rule where
    show (AssmptionRule) = "A"
    show (ConjuncRuleIntro a b) = show (sequentLineNum a)++","++show (sequentLineNum b)++" ∧I" 
    show (ConjuncRuleElimi a) = show (sequentLineNum a)++", ∧E"
    show (ImplicaRuleIntro a (Just f)) = show (sequentLineNum a)++"["++show f++"] ➞ I"
    show (ImplicaRuleIntro a Nothing) = show (sequentLineNum a)++"[] ➞ I"
    show (ImplicaRuleElimi a b) = show (sequentLineNum a)++","++show (sequentLineNum b)++" ➞ E" 
    show (RaaRule a b (Just c)) = show (sequentLineNum a)++","++show (sequentLineNum b)++"["++show c++"] RAA"
    show (RaaRule a b Nothing) = show (sequentLineNum a)++","++show (sequentLineNum b)++"[] RAA"
    show (NegationRuleIntro a (Just b)) = show (sequentLineNum a)++"["++show b++"] ¬I" 
    show (NegationRuleElimi) = "¬E"
    show (DoubleNegationRuleElimi a) = show a++" ¬¬E"
    show (OrRuleElimi a b bf c cf) = show a++","++show b++"["++show bf++"],"++show c++"["++show cf++"] ⋁E"
    show (OrRuleIntro a) = show a++" ⋁I"

