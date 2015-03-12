
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
          | ConjuncRule_Intro Proof Proof
          | ConjuncRule_Elimi Proof
          | ImplicaRule_Intro Proof Discharge
          | ImplicaRule_Elimi Proof Proof
    deriving (Show, Eq, Ord)

type Discharge = Formulae

checkAssumptionsWithDischarge :: Assumptions -> [Proof] -> Discharge -> LineNumber -> IO (Bool)
checkAssumptionsWithDischarge assumptions listOfSequents discharge lineNum =
        case listOfSequents of
            [] -> return (True)
            x:xs
                | S.isSubsetOf (S.delete discharge (sequentAssump x)) assumptions ->
                    checkAssumptionsWithDischarge assumptions xs discharge lineNum
                | otherwise -> do
                    putStrLn $ "Your sequent at line "
                        ++(show lineNum)++
                        " with assumptions "
                        ++(show assumptions)++
                        " does not contain the set of assumptions "
                        ++(show (S.delete discharge (sequentAssump x)))++
                        " spesified by the sequent's rule"
                    checkAssumptions assumptions xs lineNum
                    return (False)

checkAssumptions :: Assumptions -> [Proof] ->  LineNumber -> IO (Bool)
checkAssumptions assumptions listOfSequents lineNum =
        case listOfSequents of
            [] -> return (True)
            x:xs
                | S.isSubsetOf (sequentAssump x) assumptions ->
                    checkAssumptions assumptions xs lineNum
                | otherwise -> do
                    putStrLn $ "Your sequent at line "
                        ++(show lineNum)++
                        " with assumptions "
                        ++(show assumptions)++
                        " does not contain the set of assumptions "
                        ++(show (sequentAssump x))++
                        " spesified by the sequent's rule"
                    checkAssumptions assumptions xs lineNum
                    return (False)

assmptionRuleCheck :: Assumptions -> Formulae -> LineNumber -> IO (Bool)
assmptionRuleCheck assumptions formulae lineNum
        | S.member formulae assumptions = return True
        | otherwise = do
            putStrLn $ "Your sequent at line "
                ++(show lineNum)++
                " is not an instance of the Assumption Rule since "
                ++(show formulae)++
                " is not in the set of assumptons "
                ++(show assumptions)
            return False

conjuncRule_IntroCheck :: Assumptions -> Formulae -> LineNumber -> Proof -> Proof -> IO (Bool)
conjuncRule_IntroCheck assumptions formulae lineNum fromA fromB = do
        x <- checkAssumptions assumptions [fromA, fromB] lineNum
        y <-isInstanceOfRule
        return (x && y)

    where isInstanceOfRule :: IO (Bool)
          isInstanceOfRule = 
            case formulae of
                Sentence left Conjunction right
                    | left == (sequentFormulae fromA) && right == (sequentFormulae fromB) -> return True
                    | left == (sequentFormulae fromA) -> do
                        putStrLn $ "The sequent "
                            ++(show (sequentLineNum fromB))++
                            " referenced in your conjuction rule at line "
                            ++(show lineNum)++
                            " is not the same as the right hand side of your formulae "
                            ++(show right)++
                            ". To fix reference a sequent with the formulae "
                            ++(show right)
                        return False
                    | right == (sequentFormulae fromB) -> do
                        putStrLn $ "The sequent "
                            ++(show (sequentLineNum fromA))++
                            " referenced in your conjuction rule at line "
                            ++(show lineNum)++
                            " is not the same as the left hand side of your formulae "
                            ++(show left)++
                            ". To fix reference a sequent with the formulae "
                            ++(show left)
                        return False
                    | otherwise -> do
                        putStrLn $ "The sequents "
                            ++(show (sequentLineNum fromA))++
                            " and "
                            ++(show (sequentLineNum fromB))++
                            " referenced in your conjuction rule at line "
                            ++(show lineNum)++
                            " are not the same as the left hand side or right hand side of your formulae "
                            ++(show left)++
                            " , "
                            ++(show right)++
                            ". To fix reference a sequent with the formulae "
                            ++(show left)++
                            " and "
                            ++(show right)++
                            "."
                        return False
                _ -> do
                    putStrLn $ "The Sequent at line "
                        ++(show lineNum)++
                        " is not an instance of the conjunction introduction rule "
                        ++"it does not hav a conjunction between two formuale."
                    return False

conjuncRule_ElimiCheck :: Assumptions -> Formulae -> LineNumber -> Proof -> IO (Bool)
conjuncRule_ElimiCheck assumptions formulae lineNum fromSequent = do
        x <- checkAssumptions assumptions [fromSequent] lineNum
        y <-isInstanceOfRule
        return (x && y)

    where isInstanceOfRule :: IO (Bool)
          isInstanceOfRule = 
            case (sequentFormulae fromSequent) of
                Sentence left Conjunction right
                    | formulae == left || formulae == right -> return True
                    | otherwise -> do
                        putStrLn $ "The Sequent "
                            ++(show formulae)++
                            " does not appear in either the left or right hand side of the conjunction at "
                            ++(show (sequentLineNum fromSequent))++
                            ". Possible fix is to reference a sequent with "
                            ++(show formulae)++
                            " on either the left or right hand side of the conjunction."
                        return False
                _ -> do
                    putStrLn $ "The Sequent at line "
                        ++(show lineNum)++
                        " is not an instance of the conjunction elimination rule since it comes from "
                        ++(show fromSequent)++
                        " on line "
                        ++(show fromSequent)++
                        " which does not have a conjunction as a connective."
                    return False

implicaRule_IntroCheck :: Assumptions -> Formulae -> LineNumber -> Proof -> Discharge ->IO (Bool)
implicaRule_IntroCheck assumptions formulae lineNum fromSequent dischargedAssump = do
        x <- checkAssumptionsWithDischarge assumptions [fromSequent] dischargedAssump lineNum
        y <-isInstanceOfRule
        return (x && y)

    where isInstanceOfRule :: IO (Bool)
          isInstanceOfRule = 
            case formulae of
                Sentence left Conjunction right
                    | left == (sequentFormulae fromA) && right == (sequentFormulae fromB) -> return True
                    | left == (sequentFormulae fromA) -> do
                        putStrLn $ "The sequent "
                            ++(show (sequentLineNum fromB))++
                            " referenced in your conjuction rule at line "
                            ++(show lineNum)++
                            " is not the same as the right hand side of your formulae "
                            ++(show right)++
                            ". To fix reference a sequent with the formulae "
                            ++(show right)
                        return False
                    | right == (sequentFormulae fromB) -> do
                        putStrLn $ "The sequent "
                            ++(show (sequentLineNum fromA))++
                            " referenced in your conjuction rule at line "
                            ++(show lineNum)++
                            " is not the same as the left hand side of your formulae "
                            ++(show left)++
                            ". To fix reference a sequent with the formulae "
                            ++(show left)
                        return False
                    | otherwise -> do
                        putStrLn $ "The sequents "
                            ++(show (sequentLineNum fromA))++
                            " and "
                            ++(show (sequentLineNum fromB))++
                            " referenced in your conjuction rule at line "
                            ++(show lineNum)++
                            " are not the same as the left hand side or right hand side of your formulae "
                            ++(show left)++
                            " , "
                            ++(show right)++
                            ". To fix reference a sequent with the formulae "
                            ++(show left)++
                            " and "
                            ++(show right)++
                            "."
                        return False
                _ -> do
                    putStrLn $ "The Sequent at line "
                        ++(show lineNum)++
                        " is not an instance of the conjunction introduction rule "
                        ++"it does not hav a conjunction between two formuale."
                    return False

implicaRule_ElimiCheck :: Assumptions -> Formulae -> LineNumber -> Proof -> Proof -> IO (Bool)
implicaRule_ElimiCheck assumptions formulae lineNum fromA fromB = undefined

proofSequent :: Proof -> IO (Bool)
proofSequent (Sequent seqLineNum assumptions formulae rule) =
    case rule of
        AssmptionRule -> do
            y <- assmptionRuleCheck assumptions formulae seqLineNum
            return y
        ConjuncRule_Intro fromA fromB -> do
            y <- conjuncRule_IntroCheck assumptions formulae seqLineNum fromA fromB
            x <- proofSequent fromA
            z <- proofSequent fromB
            return (y && x && z)
        ConjuncRule_Elimi fromA -> do
            y <- conjuncRule_ElimiCheck assumptions formulae seqLineNum fromA
            x <- proofSequent fromA
            return (x && y)
        ImplicaRule_Intro fromA discharge -> do
            y <- implicaRule_IntroCheck assumptions formulae seqLineNum fromA discharge
            x <- proofSequent fromA
            return (y && x)
        ImplicaRule_Elimi fromA fromB -> do
            y <- implicaRule_ElimiCheck assumptions formulae seqLineNum fromA fromB
            x <- proofSequent fromA
            z <- proofSequent fromB
            return (y && x && z)

main :: IO ()
main = do
        return ()
