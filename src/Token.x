
{
module Token where
}

%wrapper "posn"

$digit = 0-9
$name = [a-z]
$upperCase = [A-Z]

tokens :-

    $white+ ;
    "(" {\p s -> (p, s, LeftParen)}
    ")" {\p s -> (p, s, RightParen)}
    "[" {\p s -> (p, s, LeftBrace)}
    "]" {\p s -> (p, s, RightBrace)}
    "," {\p s -> (p, s, Comma)}

    "->" {\p s -> (p, s, Implies)}
    "IMP" {\p s -> (p, s, Implies)}
    "imp" {\p s -> (p, s, Implies)}
    "→" {\p s -> (p, s, Implies)}
    "➝" {\p s -> (p, s, Implies)}
    "➞" {\p s -> (p, s, Implies)}

    "AND" {\p s -> (p, s, And)}
    "and" {\p s -> (p, s, And)}
    "⋀" {\p s -> (p, s, And)}
    "⋏" {\p s -> (p, s, And)}
    "∧" {\p s -> (p, s, And)}

    "OR" {\p s -> (p, s, Or )}
    "or" {\p s -> (p, s, Or )}
    "⋁" {\p s -> (p, s, Or )}
    "⋎" {\p s -> (p, s, Or )}

    "¬" {\p s -> (p, s, Negation)}
    "NOT" {\p s -> (p, s, Negation)}
    "not" {\p s -> (p, s, Negation)}

    "RAA" {\p s -> (p, s, Absurd)}
    "⊥" {\p s -> (p, s, Bad)}
    "bad" {\p s -> (p, s, Bad)}

    "∀" {\p s -> (p, s, ForallToken)}
    "ALL" {\p s -> (p, s, ForallToken)}
    "FORAll" {\p s -> (p, s, ForallToken)}
    "all" {\p s -> (p, s, ForallToken)}
    "forall" {\p s -> (p, s, ForallToken)}

    "∃" {\p s -> (p, s, ExistsToken)}
    "EXISTS" {\p s -> (p, s, ExistsToken)}
    "exists" {\p s -> (p, s, ExistsToken)}
    "SOME" {\p s -> (p, s, ExistsToken)}
    "some" {\p s -> (p, s, ExistsToken)}

    ":" {\p s -> (p, s, Colon)}

    $digit+ {\p s -> (p, s, Number (read s))}
    $upperCase $name+ {\p s -> (p, s, Pred (head s) (tail s))} 
    $upperCase "(" $name+ ")" {\p (x:xs) -> (p, (x:xs), Pred x (init (tail xs)))}
    $name+ {\p s -> (p, s, Name s)}

    "A" {\p s -> (p, s, Assump)}
    "E" {\p s -> (p, s, Elimination)}
    "I" {\p s -> (p, s, Introduction)}

{
data Token = Name String
           | Number Int
           | Pred Char String
           | LeftParen
           | RightParen
           | Implies
           | And
           | LeftBrace
           | RightBrace
           | Comma
           | Negation
           | Absurd
           | Or
           | Bad
           | ForallToken
           | ExistsToken
           | Colon
           | Assump
           | Elimination
           | Introduction
    deriving (Show)
}

