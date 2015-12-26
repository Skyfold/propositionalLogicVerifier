{
module Parser where
import Token
import ParserSyntax
import Syntax (Connective (..), Formulae (..), Domain (..), Quantifier (..))
import qualified Data.Set as S
}

%name prepParser
%tokentype { (AlexPosn, String, Token) }
%error { \(((AlexPn _ line column),s , _):_) -> error $ show line++":"++show column++" got unexpected "++ s }

%token
    pred { (_, _, $$@(Pred _ _)) }
    var { (_, _, (Name $$)) }
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
    forall { (_, _, (ForallToken)) }
    exists { (_, _, (ExistsToken)) }
    ':' { (_, _, (Colon)) }

%left and
%left or
%right '->'
%left not
%left forall
%left exists
%left pred
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
         | forall Domain Formulae { Scope (Forall $2) $3 }
         | exists Domain Formulae { Scope (Exists $2) $3 }
         | not Formulae { Negated $2 }
         | var { Atom $1 }
         | pred { (\(Pred upCase lowCase) -> Predecessor upCase lowCase) $1 }
         | bad { Contradiction }

Domain : '(' ListOfVariable ':' Formulae ')' { Restricted (S.fromList $2) $4 }
       | ListOfVariable { Variable (S.fromList $1) }

ListOfVariable : var { [$1] }
               | var ',' ListOfVariable { $1:$3 }

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

              | num forall I { ForallRefIntro $1 }
              | num forall E { ForallRefElimi $1 }
              | num exists I { ExistsRefIntro $1 }
              | num forall E { ExistsRefElimi $1 }

