
{
module Token where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-z]

tokens :-

    $white+ ;
    "(" {\s -> LeftParen}
    ")" {\s -> RightParen}
    "->" {\s -> Implies}
    "imp" {\s -> Implies}
    "→" {\s -> Implies}
    "➝" {\s -> Implies}
    "➞" {\s -> Implies}
    "and" {\s -> And}
    "⋀" {\s -> And}
    "⋏" {\s -> And}
    "∧" {\s -> And}
    "[" {\s -> LeftBrace}
    "]" {\s -> RightBrace}
    "," {\s -> Comma}
    "A" {\s -> Assump}
    "E" {\s -> Elimination}
    "I" {\s -> Introduction}
    "¬" {\s -> Negation}
    "or" {\s -> Or }
    "⋁" {\s -> Or }
    "⋎" {\s -> Or }
    "not" {\s -> Negation}
    "RAA" {\s -> Absurd}
    "⊥" {\s -> Bad}
    "bad" {\s -> Bad}
    $digit $digit* {\s -> Number (read s)}
    $alpha $alpha* {Variable}

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
           | Assump
           | Elimination
           | Introduction
           | Negation
           | Absurd
           | Or
           | Bad
    deriving (Show)
}

