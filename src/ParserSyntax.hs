module ParserSyntax (
    convertToTree,
    ProofLine (..),
    RuleReference (..),
    DischargeRef (..),
    ListOfSequents (..),
    ) where
import Syntax
import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.Set as S
import Data.Ord(comparing)

-- For parser

data ProofLine = Seq {
    assump :: [Int],
    getln :: LineNumber,
    getFormulae :: Formulae,
    getRule :: RuleReference
    }
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
                   | ForallRefIntro Int
                   | ForallRefElimi Int
                   | ExistsRefIntro Int
                   | ExistsRefElimi Int
                deriving (Show, Eq, Ord)

type DischargeRef = Int

type ListOfSequents = [ProofLine]

-- turn into Syntax the verifier can understand (I should make this more readable)

convertToTree :: ListOfSequents -> Proof
convertToTree list = makeProof (V.fromList $ fillList 1 $ L.sortBy (comparing getln) list)

    where makeProof :: V.Vector ProofLine -> Proof
          makeProof vec = V.last $ loeb $ fmap toProof vec

          fillList _ [] = []
          fillList n (x:xs)
            | n < getln x = (error $ "line "++show n++" does not exist"):fillList (n+1) (x:xs)
            | otherwise = x: fillList (n+1) xs

          loeb :: (V.Vector (V.Vector a -> a)) -> V.Vector a
          loeb xs = go where go = fmap ($ go) xs

          toProof :: ProofLine -> V.Vector Proof -> Proof
          toProof (Seq assum num form ref) vec =
              Sequent num (getAssumptions vec assum) form (getRules vec ref)

          getAssumptions :: V.Vector Proof -> [Int] -> Assumptions
          getAssumptions vec list = S.fromList (map (sequentFormulae . (vec !!!)) list)

          v!!!n = v V.! (n - 1)

          getRules :: V.Vector Proof -> RuleReference -> Rule
          getRules vec rule =
              case rule of
                AssmptionReference -> AssmptionRule
                ConjuncRefIntro num1 num2 ->
                    ConjuncRuleIntro (vec !!! num1) (vec !!! num2)
                ConjuncRefElimi num1 ->
                    ConjuncRuleElimi (vec !!! num1)
                ImplicaRefIntro num1 (Just num2) ->
                    ImplicaRuleIntro (vec !!! num1) (Just (sequentFormulae (vec !!! num2)))
                ImplicaRefIntro num1 Nothing ->
                    ImplicaRuleIntro (vec !!! num1) Nothing
                ImplicaRefElimi num1 num2 ->
                    ImplicaRuleElimi (vec !!! num1) (vec !!! num2)
                RaaRef num1 num2 (Just x) ->
                    RaaRule (vec !!! num1) (vec !!! num2) (Just (sequentFormulae (vec !!! x)))
                RaaRef num1 num2 Nothing ->
                    RaaRule (vec !!! num1) (vec !!! num2) Nothing
                NegationRefIntro num1 (Just x) ->
                    NegationRuleIntro (vec !!! num1) (Just (sequentFormulae (vec !!! x)))
                NegationRefIntro num1 Nothing ->
                    NegationRuleIntro (vec !!! num1)  Nothing
                NegationRefElimi -> NegationRuleElimi
                DoubleNegationRefElimi num1 ->
                    DoubleNegationRuleElimi (vec !!! num1)
                OrRefElimi num1 num2 m1 num3 m2 ->
                    OrRuleElimi (vec !!! num1) (vec !!! num2)
                        (sequentFormulae (vec !!! m1)) (vec !!! num3)
                        (sequentFormulae (vec !!! m2))
                OrRefIntro num1 ->
                    OrRuleIntro (vec !!! num1)
                ForallRefIntro num1 ->
                    ForallRuleIntro (vec !!! num1)
                ForallRefElimi num1 ->
                    ForallRuleElimi (vec !!! num1)
                ExistsRefIntro num1 ->
                    ExistsRuleElimi (vec !!! num1)

