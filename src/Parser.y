{
module Parser where
import Syntax
import Token
}

%name prepParser
%tokentype { (AlexPosn, String, Token) }
%error { \(((AlexPn _ line column),s , _):_) -> error $ show line++":"++show column++" got unexpected "++ s }

%token
    var { (_, _, (Variable $$)) }
    num { (_, _, (Number $$)) }
    '(' { (_, _, (LeftParen)) }
    ')' { (_, _, (RightParen)) }
    '->' { (_, _, (Implies)) }
    and { (_, _, (And)) }
    '[' { (_, _, (LeftBrace)) }
    ']' { (_, _, (RightBrace)) }
    ',' { (_, _, (Comma)) }
    A { (_, _, (Assump)) }
    E { (_, _, (Elimination)) }
    I { (_, _, (Introduction)) }
    not { (_, _, (Negation)) }
    RAA { (_, _, (Absurd)) }
    or { (_, _, (Or)) }
    bad { (_, _, (Bad)) }

%left and
%left or
%right '->'
%left not
%%

ListOfSequents : ProofLine { [$1] }
               | ProofLine ListOfSequents { $1:$2 }

ProofLine : Assumptions '(' num ')' Formulae RuleReference { Seq $1 $3 $5 $6 }
          | '(' num ')' Formulae RuleReference { Seq [] $2 $4 $5 }

Assumptions : num { [$1] }
            | num ',' Assumptions { $1:$3 }

Formulae : Formulae '->' Formulae { Sentence $1 Implication $3 }
         | Formulae and Formulae { Sentence $1 Conjunction $3 }
         | Formulae or Formulae { Sentence $1 Disjunction $3 }
         | '(' Formulae ')' { $2 }
         | not Formulae { Negated $2 }
         | var { Atom $1 }
         | bad { Contradiction }

RuleReference : A { AssmptionReference }
              | num ',' num and I { ConjuncRefIntro $1 $3 }
              | num and E { ConjuncRefElimi $1 }
              | num ',' num '->' E { ImplicaRefElimi $1 $3 }

              | num '[' num ']' '->' I { ImplicaRefIntro $1 (Just $3) }
              | num '[' ']' '->' I { ImplicaRefIntro $1 Nothing }

              | num ',' num '[' num ']' RAA { RaaRef $1 $3 (Just $5) }
              | num ',' num '[' ']' RAA { RaaRef $1 $3 (Nothing) }

              | num '[' num ']' not I { NegationRefIntro $1 (Just $3) }
              | num '[' ']' not I { NegationRefIntro $1 Nothing }

              | not E { NegationRefElimi }
              | num not not E { DoubleNegationRefElimi $1 }
              | num ',' num '[' num ']' ',' num '[' num ']' or E { OrRefElimi $1 $3 $5 $8 $10 } 
              | num or I { OrRefIntro $1 } 

