
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

assmptionRuleCheck :: Assumptions -> Formulae -> LineNumber -> IO ()
assmptionRuleCheck assumptions formulae lineNum
        | S.member formulae assumptions = return ()
        | otherwise = do
            putStrLn $ "Your sequent at line "
                ++(show lineNum)++
                " is not an instance of the Assumption Rule since "
                ++(show formulae)++
                " is not in the set of assumptons "
                ++(show assumptions)
            return ()

conjuncRule_IntroCheck :: Assumptions -> Formulae -> LineNumber -> Proof -> Proof -> IO (Bool)
conjuncRule_IntroCheck assumptions formulae lineNum fromA fromB = do
        return (checkAssumptions assumptions formulae lineNum [fromA, fromB]
               && checkFormuale (formulaeA == (sequentFormulae fromA), formulaeB == (sequentFormulae fromB)) formulae lineNum [fromA, fromB])


checkFormulae :: (-> Formulae -> LineNumber -> Proof -> Proof -> IO ()
checkFormulae problems formulae lineNum fromA fromB =
        case formulae of
            Sentence formulaeA connective formulaeB ->
                case problems of
                    (True, True) -> return ()
                    (False, True) -> do
                        putStrLn $ "On line "
                            ++(show lineNum)++
                            " , the left part of the "
                            ++(show connective)++
                            (show formulaeA)++
                            " does not come from "
                            ++(show (sequentFormulae fromA))++
                            " from line "
                            ++(show (sequentLineNum fromA))
                        return ()
                    (True,  False) -> do
                        putStrLn $ "On line "
                            ++(show lineNum)++
                            " , the right part of the "
                            ++(show connective)++
                            (show formulaeB)++
                            " does not come from "
                            ++(show (sequentFormulae fromB))++
                            " from line "
                            ++(show (sequentLineNum fromB))
                        return ()
                    (False, False) -> do
                        putStrLn $ "On line "
                            ++(show lineNum)++
                            " , both the right and left part of the "
                            ++(show connective)++
                            (show formulae)++
                            " does not come from either "
                            ++(show (sequentFormulae fromB))++
                            " or "
                            ++(show (sequentFormulae fromA))++
                            " from line "
                            ++(show (sequentLineNum fromB))++
                            " and line "
                            ++(show (sequentLineNum fromA))
                        return ()
            _ -> putStrLn $ "On line "
                    ++(show lineNum)++
                    " you have put the incorrect rule since "
                    ++(show formulae)++
                    " is not an instance of the conjunction introduction rule."
                return ()

checkAssumptions :: Assumptions -> Formulae -> LineNumber -> [Proof] -> IO (Bool)
checkAssumptions assumptions formulae lineNum listOfAssumptions =
        case listOfAssumptions of
            [] -> return (True)
            x:xs
                | S.isSubsetOf (sequentAssump x) assumptions -> do
                    checkAssumptions assumptions formulae lineNum xs
                | otherwise -> do
                    putStrLn $ "On line "
                        ++(show lineNum)++
                        " , you forgot to carry over the assumpton[s] "
                        ++(show (S.difference (sequentAssump x) assumptions))++
                        " from the sequent on line "
                        ++(show (sequentLineNum x))
                    checkAssumptions assumptions formulae lineNum xs
                    return (False)

conjuncRule_ElimiCheck :: Assumptions -> Formulae -> LineNumber -> Proof -> IO (Bool)
conjuncRule_ElimiCheck assumptions formulae lineNum fromSequent = undefined

implicaRule_IntroCheck :: Assumptions -> Formulae -> LineNumber -> Proof -> Discharge ->IO (Bool)
implicaRule_IntroCheck assumptions formulae lineNum formSequent dischargedAssump = undefined

implicaRule_ElimiCheck :: Assumptions -> Formulae -> LineNumber -> Proof -> Proof -> IO (Bool)
implicaRule_ElimiCheck assumptions formulae lineNum fromA fromB = undefined

proofSequent :: Proof -> IO (Bool)
proofSequent (Sequent seqLineNum assumptions formulae rule) =
    case rule of
        AssmptionRule ->
            assmptionRuleCheck assumptions formulae seqLineNum
        ConjuncRule_Intro fromA fromB -> do
            conjuncRule_IntroCheck assumptions formulae seqLineNum fromA fromB
            proofSequent fromA
            proofSequent fromB
        ConjuncRule_Elimi fromA -> do
            conjuncRule_ElimiCheck assumptions formulae seqLineNum fromA
            proofSequent fromA
        ImplicaRule_Intro fromA discharge -> do
            implicaRule_IntroCheck assumptions formulae seqLineNum fromA discharge
            proofSequent fromA
        ImplicaRule_Elimi fromA fromB -> do
            implicaRule_ElimiCheck assumptions formulae seqLineNum fromA fromB
            proofSequent fromA
            proofSequent fromB

main :: IO ()
main = do
        return ()
