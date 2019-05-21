{
module Grammar where
import Tokens
}

%name parseCalc
%tokentype { Token }
%error { parseError }
%token
    var { TokenVar $$ p }
    '=' { TokenEq p }
    '(' { TokenLParen p }
    ')' { TokenRParen p }
    and { TokenAnd p }
    apply { TokenApply p }
    ',' { TokenComma p }
    relation { TokenRelation $$ p }
    exists { TokenExist p }
    '.' { TokenDot p }
    ';' { TokenSemicolon p }

%%

Line : Line Line           { Multiple $1 $2 }
    | Values apply Exp ';' { Apply $1 $3 }

Exp : relation '(' Values ')'     { Relation $1 $3 }
    | Exp and Exp          { And $1 $3 }
    | var '=' Values          { Equals $1 $3 }
    | exists Values '.' Exp   { Exists $2 $4 }

Values : var ',' Values    { Comma $1 $3 }
    | var                  { Var $1 }

{
parseError :: [Token] -> a
parseError (x:xs) = error $ "Parse error: " ++ tokenPosn x
data Exp = Relation String Values
         | And Exp Exp
         | Equals String Values
         | Exists Values Exp
         deriving Show

data Values = Var String
         | Comma String Values
         deriving Show

data Line = Multiple Line Line
          | Apply Values Exp
        deriving Show
}
