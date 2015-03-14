import qualified Data.Set as S

data Proof = Sequent {sequentLineNum :: LineNumber, sequentAssump :: Assumptions, sequentFormulae :: Formulae, sequentRule :: Rule}
    deriving (Show, Eq, Ord)

type LineNumber = Int

type Assumptions = S.Set Formulae

data Formulae = Sentence {left :: Formulae, connective :: Connective, right :: Formulae}
              | Atom Char
    deriving (Eq, Ord, Show)

data Connective = Conjunction
                | Implication
    deriving (Eq, Ord, Show)

data Rule = AssmptionRule
          | ConjuncRuleIntro Proof Proof
          | ConjuncRuleElimi Proof
          | ImplicaRuleIntro Proof Discharge
          | ImplicaRuleElimi Proof Proof
    deriving (Show, Eq, Ord)

type Discharge = Formulae

checkAssumptionsWithDischarge :: Assumptions -> [Proof] -> Discharge -> LineNumber -> IO Bool
checkAssumptionsWithDischarge assumptions listOfSequents discharge lineNum =
        case listOfSequents of
            [] -> return True
            x:xs
                | S.isSubsetOf (S.delete discharge (sequentAssump x)) assumptions ->
                    checkAssumptionsWithDischarge assumptions xs discharge lineNum
                | otherwise -> do
                    putStrLn $ "Your sequent at line "
                        ++show lineNum++
                        " with assumptions "
                        ++show assumptions++
                        " does not contain the set of assumptions "
                        ++show (S.delete discharge (sequentAssump x))++
                        " spesified by the sequent's rule"
                    checkAssumptions assumptions xs lineNum
                    return False

checkAssumptions :: Assumptions -> [Proof] ->  LineNumber -> IO Bool
checkAssumptions assumptions listOfSequents lineNum =
        case listOfSequents of
            [] -> return True
            x:xs
                | S.isSubsetOf (sequentAssump x) assumptions ->
                    checkAssumptions assumptions xs lineNum
                | otherwise -> do
                    putStrLn $ "Your sequent at line "
                        ++show lineNum++
                        " with assumptions "
                        ++show assumptions++
                        " does not contain the set of assumptions "
                        ++show (sequentAssump x)++
                        " spesified by the sequent's rule"
                    checkAssumptions assumptions xs lineNum
                    return False

assmptionRuleCheck :: Assumptions -> Formulae -> LineNumber -> IO Bool
assmptionRuleCheck assumptions formulae lineNum
        | S.member formulae assumptions = return True
        | otherwise = do
            putStrLn $ "Your sequent at line "
                ++show lineNum++
                " is not an instance of the Assumption Rule since "
                ++show formulae++
                " is not in the set of assumptons "
                ++show assumptions
            return False

conjuncRuleIntroCheck :: Assumptions -> Formulae -> LineNumber -> Proof -> Proof -> IO Bool
conjuncRuleIntroCheck assumptions formulae lineNum fromA fromB = do
        x <- checkAssumptions assumptions [fromA, fromB] lineNum
        y <-isInstanceOfRule
        return (x && y)

    where isInstanceOfRule :: IO Bool
          isInstanceOfRule =
            case formulae of
                Sentence left Conjunction right
                    | left == sequentFormulae fromA && right == sequentFormulae fromB -> return True
                    | left == sequentFormulae fromA -> do
                        putStrLn $ "The sequent "
                            ++show (sequentLineNum fromB)++
                            " referenced in your conjuction rule at line "
                            ++show lineNum++
                            " is not the same as the right hand side of your formulae "
                            ++show right++
                            ". To fix reference a sequent with the formulae "
                            ++show right
                        return False
                    | right == sequentFormulae fromB -> do
                        putStrLn $ "The sequent "
                            ++show (sequentLineNum fromA)++
                            " referenced in your conjuction rule at line "
                            ++show lineNum++
                            " is not the same as the left hand side of your formulae "
                            ++show left++
                            ". To fix reference a sequent with the formulae "
                            ++show left
                        return False
                    | otherwise -> do
                        putStrLn $ "The sequents "
                            ++show (sequentLineNum fromA)++
                            " and "
                            ++show (sequentLineNum fromB)++
                            " referenced in your conjuction rule at line "
                            ++show lineNum++
                            " are not the same as the left hand side or right hand side of your formulae "
                            ++show left++
                            " , "
                            ++show right++
                            ". To fix reference a sequent with the formulae "
                            ++show left++
                            " and "
                            ++show right++
                            "."
                        return False
                _ -> do
                    putStrLn $ "The Sequent at line "
                        ++show lineNum++
                        " is not an instance of the conjunction introduction rule "
                        ++"it does not hav a conjunction between two formuale."
                    return False

conjuncRuleElimiCheck :: Assumptions -> Formulae -> LineNumber -> Proof -> IO Bool
conjuncRuleElimiCheck assumptions formulae lineNum fromSequent = do
        x <- checkAssumptions assumptions [fromSequent] lineNum
        y <-isInstanceOfRule
        return (x && y)

    where isInstanceOfRule :: IO Bool
          isInstanceOfRule =
            case sequentFormulae fromSequent of
                Sentence left Conjunction right
                    | formulae == left || formulae == right -> return True
                    | otherwise -> do
                        putStrLn $ "The Sequent "
                            ++show formulae++
                            " does not appear in either the left or right hand side of the conjunction at "
                            ++show (sequentLineNum fromSequent)++
                            ". Possible fix is to reference a sequent with "
                            ++show formulae++
                            " on either the left or right hand side of the conjunction."
                        return False
                _ -> do
                    putStrLn $ "The Sequent at line "
                        ++show lineNum++
                        " is not an instance of the conjunction elimination rule since it comes from "
                        ++show fromSequent++
                        " on line "
                        ++show fromSequent++
                        " which does not have a conjunction as a connective."
                    return False

implicaRuleIntroCheck :: Assumptions -> Formulae -> LineNumber -> Proof -> Formulae ->IO Bool
implicaRuleIntroCheck assumptions formulae lineNum fromSequent dischargedAssump = do
        x <- checkAssumptionsWithDischarge assumptions [fromSequent] dischargedAssump lineNum
        y <-isInstanceOfRule
        return (x && y)

    where isInstanceOfRule :: IO Bool
          isInstanceOfRule =
            case formulae of
                Sentence left Implication right
                    | left == dischargedAssump && right == formulae && S.member dischargedAssump (sequentAssump fromSequent) -> return True
                    | otherwise -> return False
                _ -> do
                    putStrLn $ "The Sequent at line "
                        ++show lineNum++
                        " is not an instance of the conjunction introduction rule "
                        ++"it does not hav a conjunction between two formuale."
                    return False

implicaRuleElimiCheck :: Assumptions -> Formulae -> LineNumber -> Proof -> Proof -> IO Bool
implicaRuleElimiCheck assumptions formulae lineNum fromA fromB = do
        x <- checkAssumptions assumptions [fromA, fromB] lineNum
        y <-isInstanceOfRule
        return (x && y)

    where isInstanceOfRule :: IO Bool
          isInstanceOfRule
            | formulae == right (sequentFormulae fromA) =
                return (sequentFormulae fromB == left (sequentFormulae fromA))
            | formulae == right (sequentFormulae fromB) =
                return (sequentFormulae fromA == left (sequentFormulae fromB))
            | otherwise = return False

proofSequent :: Proof -> IO Bool
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

main :: IO ()
main = return ()
