module Syntax where
import qualified Data.Set as S

-- For parser

data ProofLine = Seq [Int] LineNumber Formulae RuleReference
    deriving (Show, Eq, Ord)

data RuleReference = AssmptionReference
          | ConjuncRefIntro Int Int
          | ConjuncRefElimi Int
          | ImplicaRefIntro Int Int
          | ImplicaRefElimi Int Int
          | RaaRef Int Int (Maybe Int)
    deriving (Show, Eq, Ord)

type ListOfSequents = [ProofLine]

-- For actual proof system

data Proof = Sequent {sequentLineNum :: LineNumber, sequentAssump :: Assumptions, sequentFormulae :: Formulae, sequentRule :: Rule}
    deriving (Show, Eq, Ord)

type LineNumber = Int

type Assumptions = S.Set Formulae

data Formulae = Sentence Formulae Connective Formulae
              | Atom String
              | Negated Formulae
    deriving (Eq, Ord)

instance Show Formulae where
    show (Atom s) = s
    show (Sentence f1 con f2) = "("++show f1++" "++show con++" "++show f2++")"
    show (Negated f) = "¬"++show f

data Connective = Conjunction
                | Implication
    deriving (Eq, Ord)

instance Show Connective where
    show Implication = "➞"
    show Conjunction = "∧"

data Rule = AssmptionRule
          | ConjuncRuleIntro Proof Proof
          | ConjuncRuleElimi Proof
          | ImplicaRuleIntro Proof Formulae
          | ImplicaRuleElimi Proof Proof
          | RaaRule Proof Proof (Maybe Formulae)
    deriving (Show, Eq, Ord)
