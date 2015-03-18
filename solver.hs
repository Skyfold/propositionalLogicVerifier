import qualified Data.Set as S

data Proof = Sequent {sequentLineNum :: LineNumber, sequentAssump :: Assumptions, sequentFormulae :: Formulae, sequentRule :: Rule}
    deriving (Show, Eq, Ord)

type LineNumber = Int

type Assumptions = S.Set Formulae

data Formulae = Sentence Formulae Connective Formulae
              | Atom Char
    deriving (Eq, Ord, Show)

data Connective = Conjunction
                | Implication
    deriving (Eq, Ord, Show)

data Rule = AssmptionRule
          | ConjuncRuleIntro Proof Proof
          | ConjuncRuleElimi Proof
          | ImplicaRuleIntro Proof Formulae
          | ImplicaRuleElimi Proof Proof
    deriving (Show, Eq, Ord)

main :: IO Bool
main = proofSequent $ Sequent 5 (S.singleton (Atom 'p')) (Sentence (Sentence (Atom 'p') Implication (Atom 'q')) Implication (Sentence (Atom 'p') Conjunction (Atom 'q'))) (
                                ImplicaRuleIntro (
                                    (Sequent 4 (S.fromList [Sentence (Atom 'p') Implication (Atom 'q'), Atom 'p']) (Sentence (Atom 'p') Conjunction (Atom 'q')) (ConjuncRuleIntro
                                        (Sequent 1 (S.singleton (Atom 'p')) (Atom 'p') AssmptionRule)
                                        (Sequent 3 (S.fromList [Atom 'p', Sentence (Atom 'p') Implication (Atom 'q')]) (Atom 'q') (ImplicaRuleElimi
                                            (Sequent 1 (S.singleton (Atom 'p')) (Atom 'p') AssmptionRule)
                                            (Sequent 2 (S.singleton (Sentence (Atom 'p') Implication (Atom 'q'))) (Sentence (Atom 'p') Implication (Atom 'q')) AssmptionRule )
                                        ))
                                    )))
                                    (Sentence (Atom 'p') Implication (Atom 'q'))  )

type Discharge = Formulae

checkAssumptionsWithDischarge :: Assumptions -> [(Proof, Maybe Formulae)] -> LineNumber -> IO Bool
checkAssumptionsWithDischarge assumptions listOfSequentsDischarges lineNum =
        case listOfSequentsDischarges of
            [] -> return True
            ((proof, Just discharge):xs)
                | S.isSubsetOf (S.delete discharge (sequentAssump proof)) assumptions ->
                    checkAssumptionsWithDischarge assumptions xs lineNum
                | otherwise -> do
                    putStrLn $ "Your sequent at line "
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
                    putStrLn $ "Your sequent at line "
                        ++show lineNum++
                        " with assumptions "
                        ++show assumptions++
                        " does not contain the set of assumptions "
                        ++show proof++
                        " spesified by the sequent's rule"
                    checkAssumptionsWithDischarge assumptions xs lineNum
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
                Sentence l Conjunction r
                    | l == sequentFormulae fromA && r == sequentFormulae fromB -> return True
                    | l == sequentFormulae fromA -> do
                        putStrLn $ "The sequent "
                            ++show (sequentLineNum fromB)++
                            " referenced in your conjuction rule at line "
                            ++show lineNum++
                            " is not the same as the right hand side of your formulae "
                            ++show r++
                            ". To fix reference a sequent with the formulae "
                            ++show r
                        return False
                    | r == sequentFormulae fromB -> do
                        putStrLn $ "The sequent "
                            ++show (sequentLineNum fromA)++
                            " referenced in your conjuction rule at line "
                            ++show lineNum++
                            " is not the same as the left hand side of your formulae "
                            ++show l++
                            ". To fix reference a sequent with the formulae "
                            ++show l
                        return False
                    | otherwise -> do
                        putStrLn $ "The sequents "
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
                Sentence l Conjunction r
                    | formulae == l || formulae == r -> return True
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
        x <- checkAssumptionsWithDischarge assumptions [(fromSequent, Just dischargedAssump)] lineNum
        y <-isInstanceOfRule
        return (x && y)

    where isInstanceOfRule :: IO Bool
          isInstanceOfRule =
            case formulae of
                Sentence l Implication r
                    | l == dischargedAssump && r == (sequentFormulae fromSequent) -> return True
                    | l == dischargedAssump -> do
                        putStrLn $ "The right side of the Implication "
                            ++show r++
                            " in the Sequent at line "
                            ++show lineNum++
                            " is not equivalent to the forumlae "
                            ++show formulae++
                            " so it is not a valid sequent using the implication rule."
                        return False
                    | r == (sequentFormulae fromSequent) -> do
                        putStrLn $ " The left side of the Implication "
                            ++show l++
                            " in the Sequent at line "
                            ++show lineNum++
                            " is not equivalent to the formuale "
                            ++show dischargedAssump++
                            ". What you have discharged must be the same as the left side of the implication, aka "
                            ++show l++
                            " in order to be a valid form of the implication introduction rule."
                        return False
                    | otherwise -> do
                        putStrLn $ " Maybe you got your line numbers mixed up, check "
                            ++show lineNum++
                            " it could be an instace of the implication introduction rule, but neither the forumlae that you reference in line "
                            ++show (sequentLineNum fromSequent)++
                            " nor the discharge "
                            ++show dischargedAssump++
                            " make this a valid instance of the implication introduction rule."
                        return False
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
          isInstanceOfRule = case (sequentFormulae fromA) of
            Sentence l Implication r
                | formulae == r ->
                    return (sequentFormulae fromB == l)
                | otherwise -> return False

            _ -> case (sequentFormulae fromB) of
                Sentence l Implication r
                    | formulae == r ->
                        return (sequentFormulae fromA == l)
                    | otherwise ->
                        return False
                _ -> do
                    putStrLn $ "not an instance of ImplicaRuleElimi"
                    return False

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

test :: Bool
test = undefined
