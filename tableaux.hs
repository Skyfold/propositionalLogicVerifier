import Control.Monad.State.Strict
import qualified Data.Set as S

data Sequent = T Formulae
             | F Formulae
            deriving (Show, Ord, Eq)

data Formulae = Sentence Formulae Connective Formulae
              | Atom Char
              | Negated Formulae
            deriving (Show, Ord, Eq)

data Connective = And
                | Or
                | Implication
                | DoubleImplication
            deriving (Show, Ord, Eq)

solveTableaux :: [Sequent] -> S.Set Formulae -> S.Set Formulae -> State [[Formulae]] 
solveTableaux startingAssumptions trueAtoms falseAtoms = 
        case startingAssumptions of
            []
                | S.null (S.difference trueAtoms falseAtoms) -> Nothing
                | otherwise -> Just (S.difference trueAtoms falseAtoms)
            x:xs -> 
                case x of
                    T formulae ->
                        case formulae of
                            Negated bla -> 
                                case bla of
                                    Atom _ -> do
                                        y <- solveTableaux xs trueAtoms (S.insert bla falseAtoms)
                                        return y
                                    _ -> do
                                        y <- solveTableaux bla:xs trueAtoms falseAtoms
                                        return y
                            Sentence bla1 And bla2 ->undefined
                    F formulae -> undefined




main :: IO ()
main = undefined
