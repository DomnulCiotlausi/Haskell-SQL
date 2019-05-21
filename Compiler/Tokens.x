{
module Tokens where
}

%wrapper "posn"
$digit = 0-9
-- digits
$alpha = [a-z]
$upper = [A-Z]
-- alphabetic characters

tokens :-
$white+       ;
  "//".*      ;
  \=          { \p s -> TokenEq p }
  \(          { \p s -> TokenLParen p }
  \)          { \p s -> TokenRParen p }
  and         { \p s -> TokenAnd p }
  apply       { \p s -> TokenApply p }
  \,          { \p s -> TokenComma p }
  \exists     { \p s -> TokenExist p }
  \.          { \p s -> TokenDot p }
  \;          { \p s -> TokenSemicolon p }
  $alpha [$alpha $digit \_ \’]*   { \p s -> TokenVar s p }
  $upper [$upper $digit \_ \’]*   { \p s -> TokenRelation s p }

{
-- Each action has type :: String -> Token
-- The token type:
data Token =
  TokenEq                   AlexPosn     |
  TokenLParen               AlexPosn     |
  TokenRParen               AlexPosn     |
  TokenAnd                  AlexPosn     |
  TokenApply                AlexPosn     |
  TokenComma                AlexPosn     |
  TokenRelation String      AlexPosn     |
  TokenVar String           AlexPosn     |
  TokenExist                AlexPosn     |
  TokenDot                  AlexPosn     |
  TokenSemicolon            AlexPosn
  deriving (Eq,Show)

tokenPosn (TokenEq (AlexPn a b c)) = "Error at line " ++ show b ++ ", column " ++ show c ++ "."
tokenPosn (TokenLParen (AlexPn a b c)) = "Error at line " ++ show b ++ ", column " ++ show c ++ "."
tokenPosn (TokenRParen (AlexPn a b c)) = "Error at line " ++ show b ++ ", column " ++ show c ++ "."
tokenPosn (TokenAnd (AlexPn a b c)) = "Error at line " ++ show b ++ ", column " ++ show c ++ "."
tokenPosn (TokenApply  (AlexPn a b c)) = "Error at line " ++ show b ++ ", column " ++ show c ++ "."
tokenPosn (TokenComma (AlexPn a b c)) = "Error at line " ++ show b ++ ", column " ++ show c ++ "."
tokenPosn (TokenVar _ (AlexPn a b c)) = "Error at line " ++ show b ++ ", column " ++ show c ++ "."
tokenPosn (TokenRelation _ (AlexPn a b c)) = "Error at line " ++ show b ++ ", column " ++ show c ++ "."
tokenPosn (TokenExist (AlexPn a b c)) = "Error at line " ++ show b ++ ", column " ++ show c ++ "."
tokenPosn (TokenDot (AlexPn a b c)) = "Error at line " ++ show b ++ ", column " ++ show c ++ "."
tokenPosn (TokenSemicolon (AlexPn a b c)) = "Error at line " ++ show b ++ ", column " ++ show c ++ "."

}
