
{
module Token where
}

%wrapper "posn"

$digit = 0-9
$alphaL = [a-z]
$alphaC = [A-Z]

tokens :-

    $white+ ;
    "(" {\p s -> (p, s, LeftParen)}
    ")" {\p s -> (p, s, RightParen)}
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
    "[" {\p s -> (p, s, LeftBrace)}
    "]" {\p s -> (p, s, RightBrace)}
    "," {\p s -> (p, s, Comma)}
    "A" {\p s -> (p, s, Assump)}
    "E" {\p s -> (p, s, Elimination)}
    "I" {\p s -> (p, s, Introduction)}
    "¬" {\p s -> (p, s, Negation)}
    "OR" {\p s -> (p, s, Or )}
    "or" {\p s -> (p, s, Or )}
    "⋁" {\p s -> (p, s, Or )}
    "⋎" {\p s -> (p, s, Or )}
    "NOT" {\p s -> (p, s, Negation)}
    "not" {\p s -> (p, s, Negation)}
    "RAA" {\p s -> (p, s, Absurd)}
    "⊥" {\p s -> (p, s, Bad)}
    "bad" {\p s -> (p, s, Bad)}
    $digit $digit* {\p s -> (p, s, Number (read s))}
    $alphaL $alphaL* {\ p s -> (p, s, Variable s)}
    $alphaC $alphaL* {\ p s -> (p, s, Func s)}

{
data Token = Variable String
           | Func String
           | Number Int
           | LeftParen
           | RightParen
           | Implies
           | And
           | LeftBrace
           | RightBrace
           | Comma
           | Assump
           | Elimination
           | Introduction
           | Negation
           | Absurd
           | Or
           | Bad
    deriving (Show)
}

