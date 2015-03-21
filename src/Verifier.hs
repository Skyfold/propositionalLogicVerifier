import qualified Data.Set as S
import System.Environment
import Syntax
import Parser
import Token (alexScanTokens)
import qualified Data.Vector as V
import Control.Monad.Writer.Lazy
import qualified Data.List as L

{-        mapM_ print res  
        print (convertToTree res)
        print ((convertToTree res) == test2)
        -} 

convertToTree :: ListOfSequents -> Proof
convertToTree list = makeProof (V.fromList list)

    where makeProof :: V.Vector ProofLine -> Proof
          makeProof vec = V.last $ loeb $ fmap toProof vec

          loeb :: (Functor f) => (f (f a -> a)) -> f a
          loeb xs = go where go = fmap ($ go) xs

          toProof :: ProofLine -> V.Vector Proof -> Proof
          toProof (Seq assum num form ref) vec = Sequent num (getAssumptions vec assum) form (getRules vec ref)

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
                    OrRuleElimi (vec !!! num1) (vec !!! num2) (sequentFormulae (vec !!! m1)) (vec !!! num3) (sequentFormulae (vec !!! m2))
                OrRefIntro num1 ->
                    OrRuleIntro (vec !!! num1) 

checkAssumptionsWithDischarge :: Assumptions -> [(Proof, Maybe Formulae)] -> LineNumber -> Writer [String] Bool
checkAssumptionsWithDischarge assumptions listOfSequentsDischarges lineNum =
        case listOfSequentsDischarges of
            [] -> return True
            ((proof, Just discharge):xs)
                | S.isSubsetOf (S.delete discharge (sequentAssump proof)) assumptions ->
                    checkAssumptionsWithDischarge assumptions xs lineNum
                | otherwise -> do
                    reportError $ "Your sequent at line "
                        ++show lineNum++
                        " with assumptions "
                        ++ppAssump assumptions++
                        " does not contain the set of assumptions "
                        ++show (S.delete discharge (sequentAssump proof))++
                        " spesified by the sequent's rule"
                    checkAssumptionsWithDischarge assumptions xs lineNum
                    return False
            ((proof, Nothing):xs)
                | S.isSubsetOf (sequentAssump proof) assumptions ->
                    checkAssumptionsWithDischarge assumptions xs lineNum
                | otherwise -> do
                    reportError $ "Your sequent at line "
                        ++show lineNum++
                        " with assumptions "
                        ++ppAssump assumptions++
                        " does not contain the set of assumptions "
                        ++show proof++
                        " spesified by the sequent's rule"
                    checkAssumptionsWithDischarge assumptions xs lineNum
                    return False


reportError str = tell [str]

checkAssumptions :: Assumptions -> [Proof] ->  LineNumber -> Writer [String] Bool
checkAssumptions assumptions listOfSequents lineNum =
        case listOfSequents of
            [] -> return True
            x:xs
                | S.isSubsetOf (sequentAssump x) assumptions ->
                    checkAssumptions assumptions xs lineNum
                | otherwise -> do
                    reportError $ "Your sequent at line "
                        ++show lineNum++
                        " with assumptions "
                        ++ppAssump assumptions++
                        " does not contain the set of assumptions "
                        ++show (sequentAssump x)++
                        " spesified by the sequent's rule"
                    checkAssumptions assumptions xs lineNum
                    return False

assmptionRuleCheck :: Assumptions -> Formulae -> LineNumber -> Writer [String] Bool
assmptionRuleCheck assumptions formulae lineNum
        | S.member formulae assumptions = return True
        | otherwise = do
            reportError $ "Your sequent at line "
                ++show lineNum++
                " is not an instance of the Assumption Rule since "
                ++show formulae++
                " is not in the set of assumptons "
                ++ppAssump assumptions
            return False

conjuncRuleIntroCheck :: Assumptions -> Formulae -> LineNumber -> Proof -> Proof -> Writer [String] Bool
conjuncRuleIntroCheck assumptions formulae lineNum fromA fromB = do
        x <- checkAssumptions assumptions [fromA, fromB] lineNum
        y <-isInstanceOfRule
        return (x && y)

    where isInstanceOfRule :: Writer [String] Bool
          isInstanceOfRule =
            case formulae of
                Sentence l Conjunction r
                    | l == sequentFormulae fromA && r == sequentFormulae fromB -> return True
                    | l == sequentFormulae fromA -> do
                        reportError $ "The sequent "
                            ++show (sequentLineNum fromB)++
                            " referenced in your conjuction rule at line "
                            ++show lineNum++
                            " is not the same as the right hand side of your formulae "
                            ++show r++
                            ". To fix reference a sequent with the formulae "
                            ++show r
                        return False
                    | r == sequentFormulae fromB -> do
                        reportError $ "The sequent "
                            ++show (sequentLineNum fromA)++
                            " referenced in your conjuction rule at line "
                            ++show lineNum++
                            " is not the same as the left hand side of your formulae "
                            ++show l++
                            ". To fix reference a sequent with the formulae "
                            ++show l
                        return False
                    | otherwise -> do
                        reportError $ "The sequents "
                            ++show (sequentLineNum fromA)++
                            " and "
                            ++show (sequentLineNum fromB)++
                            " referenced in your conjuction rule at line "
                            ++show lineNum++
                            " are not the same as the left hand side or right hand side of your formulae "
                            ++show l++
                            " , "
                            ++show r++
                            ". To fix reference a sequent with the formulae "
                            ++show l++
                            " and "
                            ++show r++
                            "."
                        return False
                _ -> do
                    reportError $ "The Sequent at line "
                        ++show lineNum++
                        " is not an instance of the conjunction introduction rule "
                        ++"it does not hav a conjunction between two formuale."
                    return False

conjuncRuleElimiCheck :: Assumptions -> Formulae -> LineNumber -> Proof -> Writer [String] Bool
conjuncRuleElimiCheck assumptions formulae lineNum fromSequent = do
        x <- checkAssumptions assumptions [fromSequent] lineNum
        y <-isInstanceOfRule
        return (x && y)

    where isInstanceOfRule :: Writer [String] Bool
          isInstanceOfRule =
            case sequentFormulae fromSequent of
                Sentence l Conjunction r
                    | formulae == l || formulae == r -> return True
                    | otherwise -> do
                        reportError $ show lineNum++ " : neither "
                            ++show l++
                            " nor "
                            ++show r++
                            " contains "
                            ++show formulae
                        return False
                _ -> do
                    reportError $ show lineNum++ " : "
                        ++show formulae++
                        " ⊬ "
                        ++show (sequentFormulae fromSequent)++
                        " from ∧ E rule. You need "
                        ++show (Sentence formulae Conjunction (Atom "x"))++
                        " where x can be anything."
                    return False

implicaRuleIntroCheck :: Assumptions -> Formulae -> LineNumber -> Proof -> Maybe Formulae ->Writer [String] Bool
implicaRuleIntroCheck assumptions formulae lineNum fromSequent dischargedAssump = do
        x <- checkAssumptionsWithDischarge assumptions [(fromSequent, dischargedAssump)] lineNum
        y <-isInstanceOfRule
        return (x && y)

    where isInstanceOfRule :: Writer [String] Bool
          isInstanceOfRule =
            case formulae of
                Sentence l Implication r -> case dischargedAssump of
                    Just ds
                        | l == ds && r == (sequentFormulae fromSequent) -> return True
                        | otherwise -> do 
                            when (not (r == (sequentFormulae fromSequent))) $
                                reportError $ show lineNum++ " : "
                                    ++show r++
                                    " found where "
                                    ++show (sequentFormulae fromSequent)++
                                    " was expected in "
                                    ++show formulae
                            when (not (l == ds)) $
                                reportError $ show lineNum++ " : "
                                    ++show l++
                                    " found where "
                                    ++show dischargedAssump++
                                    " was expected in "
                                    ++show formulae
                            return False
                    Nothing
                        | r == (sequentFormulae fromSequent) -> return True
                        | otherwise -> do
                            when (not (r == (sequentFormulae fromSequent))) $
                                reportError $ show lineNum++ " : "
                                    ++show r++
                                    " found where "
                                    ++show (sequentFormulae fromSequent)++
                                    " was expected in "
                                    ++show formulae
                            return False
                _ -> do
                    reportError $ "The Sequent at line "
                        ++show lineNum++
                        " is not an instance of the conjunction introduction rule "
                        ++"it does not have a conjunction between two formuale."
                    return False

implicaRuleElimiCheck :: Assumptions -> Formulae -> LineNumber -> Proof -> Proof -> Writer [String] Bool
implicaRuleElimiCheck assumptions formulae lineNum fromA fromB = do
        x <- checkAssumptions assumptions [fromA, fromB] lineNum
        y <-isInstanceOfRule
        return (x && y)

    where isInstanceOfRule :: Writer [String] Bool
          isInstanceOfRule = case (sequentFormulae fromA) of
            Sentence l Implication r
                | formulae == r ->
                    return (sequentFormulae fromB == l)
                | otherwise -> do
                    reportError $ show lineNum++ " : "
                        ++show formulae++
                        " found where "
                        ++show r++
                        " was expected."
                    return False

            _ -> case (sequentFormulae fromB) of
                Sentence l Implication r
                    | formulae == r ->
                        return (sequentFormulae fromA == l)
                    | otherwise -> do
                        reportError $ show lineNum++ " : "
                            ++show r++
                            " found where "
                            ++show formulae++
                            " was expected."
                        return False
                _ -> do
                    reportError $ show lineNum++ " : neither "
                        ++show (sequentFormulae fromA)++
                        " nor "
                        ++show (sequentFormulae fromB)++
                        " have ➞ as their main connective, wrong rule"
                    return False

raaRuleCheck :: Assumptions -> Formulae -> LineNumber -> Proof -> Proof -> Maybe Formulae -> Writer [String] Bool
raaRuleCheck assumptions formulae lineNum fromA fromB maybeFormulae = do
        x <- checkAssumptionsWithDischarge assumptions [(fromA, maybeFormulae), (fromB, maybeFormulae)] lineNum
        y <- isInstanceOfRule
        z <- checkFormulae
        return (x && y && z) 

    where isInstanceOfRule :: Writer [String] Bool
          isInstanceOfRule = 
              case (sequentFormulae fromA) of
                Negated x
                    | x == (sequentFormulae fromB) -> return True
                    | otherwise -> do
                        reportError $ show lineNum++ " : "
                            ++show (sequentFormulae fromA)++
                            " from "
                            ++show (sequentLineNum fromA)++
                            " is not the negation of "
                            ++show (sequentFormulae fromB)++
                            " from "
                            ++show (sequentLineNum fromB)
                        return False

                _ -> case (sequentFormulae fromB) of
                    Negated x
                            | x == (sequentFormulae fromA) -> return True
                            | otherwise -> do
                                reportError $ show lineNum++ " : "
                                    ++show (sequentFormulae fromB)++
                                    " from "
                                    ++show (sequentLineNum fromB)++
                                    " is not the negation of "
                                    ++show (sequentFormulae fromA)++
                                    " from "
                                    ++show (sequentLineNum fromA)
                                return False
                    _ -> do
                        reportError $ show lineNum++ " : neither "
                            ++show (sequentFormulae fromB)++
                            " nor "
                            ++show (sequentFormulae fromA)++
                            " contain ¬ outside of their main connective."
                        return False

          checkFormulae :: Writer [String] Bool
          checkFormulae = case maybeFormulae of
                            Just x
                              | Negated x == formulae -> return True
                              | otherwise -> do
                                  reportError $ show lineNum++ " : "
                                      ++show formulae++
                                      " should be "
                                      ++show (Negated x)
                                  return False
                            Nothing -> return True

negationRuleIntroCheck :: Assumptions -> Formulae -> LineNumber -> Proof -> Maybe Formulae -> Writer [String] Bool
negationRuleIntroCheck assumptions formulae lineNum fromA maybeFormulae = do
        x <- checkAssumptionsWithDischarge assumptions [(fromA, maybeFormulae)] lineNum
        y <- isInstanceOfRule
        z <- checkFormulae
        return (x && y && z)

    where isInstanceOfRule :: Writer [String] Bool
          isInstanceOfRule 
            | (sequentFormulae fromA) == Contradiction = return True
            | otherwise = do
                reportError $ show lineNum++ " : "
                    ++show (sequentFormulae fromA)++
                    " at line "
                    ++show (sequentLineNum fromA)++
                    " should be ⊥ "
                return False

          checkFormulae :: Writer [String] Bool
          checkFormulae = case maybeFormulae of
                            Just x
                              | Negated x == formulae -> return True
                              | otherwise -> do
                                  reportError $ show lineNum++ " : "
                                      ++show formulae++
                                      " should be "
                                      ++show (Negated x)
                                  return False
                            Nothing -> return True

negationRuleElimCheck :: Assumptions -> Formulae -> LineNumber -> Writer [String] Bool
negationRuleElimCheck assumptions formulae lineNum
    | L.any (\a -> S.member (Negated a) assumptions) (S.toList assumptions) =
      return True
    | otherwise = do
        reportError $ show lineNum++ " : "
            ++show (S.toList assumptions)++
            " does not have a contradiction "
        return False

doubleNegationRuleElimiCheck :: Assumptions -> Formulae ->  LineNumber -> Proof -> Writer [String] Bool
doubleNegationRuleElimiCheck assumptions formulae lineNum fromA = do
        x <- checkAssumptions assumptions [fromA] lineNum
        y <-isInstanceOfRule
        return (x && y)

    where isInstanceOfRule :: Writer [String] Bool
          isInstanceOfRule = case (sequentFormulae fromA) of
                               Negated (Negated x)
                                    | x == formulae -> return True
                                    | otherwise -> do
                                        reportError $ show lineNum++ " : "
                                            ++show (sequentFormulae fromA)++
                                            " ⊬ "
                                            ++show formulae++
                                            ". You need "
                                            ++show (Negated (Negated formulae))++
                                            " ⊢ "
                                            ++show formulae
                                        return False
                               _ -> do  
                                   reportError $ show lineNum++ " : "
                                        ++show (sequentFormulae fromA)++
                                        " is not of the form ¬¬p "
                                   return False

orRuleElimiCheck :: Assumptions -> Formulae -> LineNumber -> Proof -> Proof -> Formulae -> Proof -> Formulae -> Writer [String] Bool
orRuleElimiCheck assumptions formulae lineNum orSeq fromLeft leftDischarge fromRight rightDischarge = do
        x <- checkAssumptionsWithDischarge assumptions [(fromLeft, Just leftDischarge), (fromRight, Just rightDischarge)] lineNum
        p <- checkFirstOrSequetAssumptions
        y <- isInstanceOfRule
        z <- checkFormulae
        return (x && y && z && p)

    where isInstanceOfRule :: Writer [String] Bool
          isInstanceOfRule = 
              case (sequentFormulae orSeq) of
                Sentence l Disjunction r
                    | (S.member leftDischarge (sequentAssump fromLeft) &&
                        ((l == leftDischarge && r == rightDischarge) || 
                        (r == leftDischarge && l == rightDischarge))
                        && (S.member rightDischarge (sequentAssump fromRight))) ->
                            return True
                    | otherwise -> do
                        when (not (S.member leftDischarge (sequentAssump fromLeft))) $
                            reportError $ show lineNum++ " : Discharge "
                            ++show leftDischarge++
                            " is not in the assumptions from line "
                            ++show (sequentLineNum fromLeft)

                        when (not (S.member rightDischarge (sequentAssump fromRight))) $
                            reportError $ show lineNum++ " : Discharge "
                            ++show rightDischarge++
                            " is not in the assumptions from line "
                            ++show (sequentLineNum fromRight)

                        when (not (l == leftDischarge || r == leftDischarge)) $
                            reportError $ show lineNum++ " : Discharge "
                            ++show leftDischarge++
                            " needs to be either "
                            ++show l++
                            " or "
                            ++show r++
                            " since you referenced line "
                            ++show (sequentLineNum orSeq)++
                            " with "
                            ++show (sequentFormulae orSeq)

                        when (not (l == rightDischarge || r == rightDischarge)) $
                            reportError $ show lineNum++ " : Discharge "
                            ++show rightDischarge++
                            " needs to be either "
                            ++show l++
                            " or "
                            ++show r++
                            " since you referenced line "
                            ++show (sequentLineNum orSeq)++
                            " with "
                            ++show (sequentFormulae orSeq)
                        return False 

                _ -> do
                    reportError $ show lineNum++ " : The formulae "
                        ++show (sequentFormulae orSeq)++
                        " from line "
                        ++show (sequentLineNum orSeq)++
                        " should be of the form (a ⋁ b) "
                    return False

          checkFormulae :: Writer [String] Bool
          checkFormulae
            | formulae == (sequentFormulae fromLeft) && formulae == (sequentFormulae fromRight) =
              return True
            | otherwise = do
                when (not (formulae == (sequentFormulae fromLeft))) $
                    reportError $ show lineNum++ " : The formulae "
                        ++show (sequentFormulae fromLeft)++
                        " from line "
                        ++show (sequentLineNum fromLeft)++
                        " should be "
                        ++show formulae
                when (not (formulae == (sequentFormulae fromRight))) $
                    reportError $ show lineNum++ " : The formulae "
                        ++show (sequentFormulae fromRight)++
                        " from line "
                        ++show (sequentLineNum fromRight)++
                        " should be "
                        ++show formulae
                return False 

          checkFirstOrSequetAssumptions :: Writer [String] Bool
          checkFirstOrSequetAssumptions
            | S.isSubsetOf (S.delete leftDischarge (S.delete rightDischarge (sequentAssump orSeq))) assumptions =
              return True
            | otherwise = do
                reportError $ show lineNum++ " : You forgot to copy over assumption[s] " 
                    ++show (S.difference (sequentAssump orSeq) 
                        (S.intersection (sequentAssump orSeq) assumptions))++
                    " from line "
                    ++show (sequentLineNum orSeq)++
                    "." 
                return False

orRuleIntroCheck :: Assumptions -> Formulae -> LineNumber -> Proof -> Writer [String] Bool
orRuleIntroCheck assumptions formulae lineNum fromA = do
        x <- checkAssumptions assumptions [fromA] lineNum
        y <-isInstanceOfRule
        return (x && y)

    where isInstanceOfRule :: Writer [String] Bool
          isInstanceOfRule = 
              case formulae of
                Sentence l Disjunction r
                    | l == (sequentFormulae fromA) || r == (sequentFormulae fromA) ->
                      return True
                    | otherwise -> do
                        reportError $ show lineNum++ " : neither "
                            ++show l++
                            " nor "
                            ++show r++
                            " ≣ "
                            ++show (sequentFormulae fromA)
                        return False
                _ -> do
                    reportError $ show lineNum++ " : "
                        ++show formulae++
                        " needs to be of the form (p ⋁ q)."
                    return False

proofSequent :: Proof -> Writer [String] Bool
proofSequent (Sequent seqLineNum assumptions formulae rule) =
    case rule of
        AssmptionRule -> assmptionRuleCheck assumptions formulae seqLineNum
        ConjuncRuleIntro fromA fromB -> do
            y <- conjuncRuleIntroCheck assumptions formulae seqLineNum fromA fromB
            x <- proofSequent fromA
            z <- proofSequent fromB
            return (y && x && z)
        ConjuncRuleElimi fromA -> do
            y <- conjuncRuleElimiCheck assumptions formulae seqLineNum fromA
            x <- proofSequent fromA
            return (x && y)
        ImplicaRuleIntro fromA discharge -> do
            y <- implicaRuleIntroCheck assumptions formulae seqLineNum fromA discharge
            x <- proofSequent fromA
            return (y && x)
        ImplicaRuleElimi fromA fromB -> do
            y <- implicaRuleElimiCheck assumptions formulae seqLineNum fromA fromB
            x <- proofSequent fromA
            z <- proofSequent fromB
            return (y && x && z)
        RaaRule fromA fromB maybeFormulae -> do
            y <- raaRuleCheck assumptions formulae seqLineNum fromA fromB maybeFormulae
            x <- proofSequent fromA
            z <- proofSequent fromB
            return (y && x && z)
        NegationRuleIntro fromA maybeFormulae -> do
            y <- negationRuleIntroCheck assumptions formulae seqLineNum fromA maybeFormulae
            x <- proofSequent fromA
            return (y && x)
        NegationRuleElimi -> negationRuleElimCheck assumptions formulae seqLineNum 
        DoubleNegationRuleElimi fromA -> do
            y <- doubleNegationRuleElimiCheck assumptions formulae seqLineNum fromA 
            x <- proofSequent fromA
            return (y && x)
        OrRuleElimi orSeq fromLeft leftDischarge fromRight rightDischarge -> do
            y <- orRuleElimiCheck assumptions formulae seqLineNum orSeq fromLeft leftDischarge fromRight rightDischarge 
            x <- proofSequent orSeq
            z <- proofSequent fromLeft
            p <- proofSequent fromRight
            return (y && x && z && p)
        OrRuleIntro fromA -> do
            y <- orRuleIntroCheck assumptions formulae seqLineNum fromA 
            x <- proofSequent fromA
            return (y && x)

main :: IO ()
main = do
        filename:_ <- getArgs
        program <- readFile filename
--        mapM_ print (alexScanTokens program)
        let proof = convertToTree $ prepParser $ alexScanTokens program
        case runWriter (proofSequent proof) of
          (b, xs) -> do
              mapM_ putStrLn $ reverse xs
              print b
