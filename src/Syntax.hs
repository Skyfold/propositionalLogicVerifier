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

data Proof = Sequent {sequentLineNum :: LineNumber, sequentAssump :: Assumptions, sequentFormulae :: Formulae, sequentRule :: Rule}
    deriving (Eq, Ord)

instance Show Proof where
    show (Sequent ln as for rule) = show as++" ("++show ln++") "++show for++" "++show rule

type LineNumber = Int

type Assumptions = S.Set Formulae

data Formulae = Sentence Formulae Connective Formulae
              | Atom String
              | Negated Formulae
              | Contradiction
    deriving (Eq, Ord)

instance Show Formulae where
    show (Atom s) = s
    show (Sentence f1 con f2) = "("++show f1++" "++show con++" "++show f2++")"
    show (Negated f) = "¬"++show f

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

