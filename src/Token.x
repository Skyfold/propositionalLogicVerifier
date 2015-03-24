
{
module Token where
}

%wrapper "posn"

$digit = 0-9
$lowerCase = [a-z]
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

    "∀" {\p s -> (p, s, Forall)}
    "ALL" {\p s -> (p, s, Forall)}
    "FORAll" {\p s -> (p, s, Forall)} 
    "all" {\p s -> (p, s, Forall)}
    "forall" {\p s -> (p, s, Forall)}

    "∃" {\p s -> (p, s, Exists)}
    "EXISTS" {\p s -> (p, s, Exists)}
    "exists" {\p s -> (p, s, Exists)}
    "SOME" {\p s -> (p, s, Exists)}
    "some" {\p s -> (p, s, Exists)}

    $digit $digit* {\p s -> (p, s, Number (read s))}
    $lowerCase $lowerCase* {\ p s -> (p, s, Variable s)}
    $upperCase $lowerCase $lowerCase* {\ p s -> (p, s, Variable s)}

    "A" {\p s -> (p, s, Assump)}
    "E" {\p s -> (p, s, Elimination)}
    "I" {\p s -> (p, s, Introduction)}

{
data Token = Variable String
           | Number Int
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
           | Forall
           | Exists
           | Assump
           | Elimination
           | Introduction
    deriving (Show)
}

