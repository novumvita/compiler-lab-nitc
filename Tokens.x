{
module Tokens where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+                       ;
  "--".*                        ;
  "begin"                       { \s -> TokenBegin }
  "end"                         { \s -> TokenEnd }
  "read"                        { \s -> TokenRead }
  "write"                       { \s -> TokenWrite }
  "if"                          { \s -> TokenIf }
  "then"                        { \s -> TokenThen }
  "else"                        { \s -> TokenElse }
  "endif"                       { \s -> TokenEndIf }
  "while"                       { \s -> TokenWhile }
  "endwhile"                    { \s -> TokenEndWhile}
  "do"                          { \s -> TokenDo }
  "break"                       { \s -> TokenBreak }
  "continue"                    { \s -> TokenContinue }
  "decl"                        { \s -> TokenDecl }
  "enddecl"                     { \s -> TokenEndDecl }
  "int"                         { \s -> TokenTypeInt }
  "str"                         { \s -> TokenTypeString }
  \,                            { \s -> TokenComma }
  $digit+                       { \s -> TokenDig (read s) }
  \=                            { \s -> TokenEq }
  \+                            { \s -> TokenPlus }
  \-                            { \s -> TokenMinus }
  \*                            { \s -> TokenTimes }
  \/                            { \s -> TokenDiv }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  \[                            { \s -> TokenLSquare }
  \]                            { \s -> TokenRSquare }
  \;                            { \s -> TokenSemiColon}
  \<                            { \s -> TokenLT }
  \>                            { \s -> TokenGT }
  \!                            { \s -> TokenNot }
  \&                            { \s -> TokenAnd }
  $alpha [$alpha $digit \_ \']* { \s -> TokenID s }
  \"[^\"]*\"                    { \s -> TokenString s }

{

-- The token type:
data Token = TokenBegin
           | TokenEnd
           | TokenRead
           | TokenWrite
           | TokenDig Int
           | TokenID String
           | TokenEq
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDiv
           | TokenLParen
           | TokenRParen
           | TokenLSquare
           | TokenRSquare
           | TokenSemiColon
           | TokenLT
           | TokenGT
           | TokenNot
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenEndIf
           | TokenWhile
           | TokenEndWhile
           | TokenDo
           | TokenBreak
           | TokenContinue
           | TokenDecl
           | TokenEndDecl
           | TokenTypeInt
           | TokenTypeString
           | TokenComma
           | TokenString String
           | TokenAnd
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
