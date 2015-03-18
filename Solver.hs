import qualified Data.Set as S
import System.Environment
import Syntax
import Parser
import Token (alexScanTokens)
import qualified Data.Vector as V
import Control.Monad.Writer.Lazy

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
          getRules vec rule = case rule of
                                     AssmptionReference -> AssmptionRule
                                     ConjuncRefIntro num1 num2 -> 
                                       ConjuncRuleIntro (vec !!! num1) (vec !!! num2)
                                     ConjuncRefElimi num1 -> 
                                       ConjuncRuleElimi (vec !!! num1) 
                                     ImplicaRefIntro num1 num2 -> 
                                       ImplicaRuleIntro (vec !!! num1) (sequentFormulae (vec !!! num2))
                                     ImplicaRefElimi num1 num2 -> 
                                       ImplicaRuleElimi (vec !!! num1) (vec !!! num2)
                                     RaaRef num1 num2 m-> 
                                        case m of
                                          Just x -> 
                                            RaaRule (vec !!! num1) (vec !!! num2) (Just (sequentFormulae (vec !!! x)))
                                          Nothing ->
                                            RaaRule (vec !!! num1) (vec !!! num2) Nothing 

                                      


test2 = Sequent 5 (S.singleton (Atom "p")) (Sentence (Sentence (Atom "p") Implication (Atom "q")) Implication (Sentence (Atom "p") Conjunction (Atom "q"))) (
                                ImplicaRuleIntro (
                                    (Sequent 4 (S.fromList [Sentence (Atom "p") Implication (Atom "q"), Atom "p"]) (Sentence (Atom "p") Conjunction (Atom "q")) (ConjuncRuleIntro
                                        (Sequent 1 (S.singleton (Atom "p")) (Atom "p") AssmptionRule)
                                        (Sequent 3 (S.fromList [Atom "p", Sentence (Atom "p") Implication (Atom "q")]) (Atom "q") (ImplicaRuleElimi
                                            (Sequent 1 (S.singleton (Atom "p")) (Atom "p") AssmptionRule)
                                            (Sequent 2 (S.singleton (Sentence (Atom "p") Implication (Atom "q"))) (Sentence (Atom "p") Implication (Atom "q")) AssmptionRule )
                                        ))
                                    )))
                                    (Sentence (Atom "p") Implication (Atom "q"))  )


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
                        ++show assumptions++
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
                        ++show assumptions++
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
                        ++show assumptions++
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
                ++show assumptions
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

implicaRuleIntroCheck :: Assumptions -> Formulae -> LineNumber -> Proof -> Formulae ->Writer [String] Bool
implicaRuleIntroCheck assumptions formulae lineNum fromSequent dischargedAssump = do
        x <- checkAssumptionsWithDischarge assumptions [(fromSequent, Just dischargedAssump)] lineNum
        y <-isInstanceOfRule
        return (x && y)

    where isInstanceOfRule :: Writer [String] Bool
          isInstanceOfRule =
            case formulae of
                Sentence l Implication r
                    | l == dischargedAssump && r == (sequentFormulae fromSequent) -> return True
                    | otherwise -> do 
                        when (not (r == (sequentFormulae fromSequent))) $
                            reportError $ show lineNum++ " : "
                                ++show r++
                                " found where "
                                ++show (sequentFormulae fromSequent)++
                                " was expected in "
                                ++show formulae
                        when (not (l == dischargedAssump)) $
                            reportError $ show lineNum++ " : "
                                ++show l++
                                " found where "
                                ++show dischargedAssump++
                                " was expected in "
                                ++show formulae
                        return False
                _ -> do
                    reportError $ "The Sequent at line "
                        ++show lineNum++
                        " is not an instance of the conjunction introduction rule "
                        ++"it does not hav a conjunction between two formuale."
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
        y <-isInstanceOfRule
        return (x && y)

    where isInstanceOfRule :: Writer [String] Bool
          isInstanceOfRule = case (sequentFormulae fromA) of
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

