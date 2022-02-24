{
module Lexer (Token (..), LexOut (..), scan) where
}

%wrapper "posn"

@str = [a-zA-Z][a-zA-Z0-9]*
@num = [0-9]+

tokens :-

  $white+                               ;
  "#".*                                 ;

  "("                                   { \p s -> lexOut p LLParen }
  ")"                                   { \p s -> lexOut p LRParen }

  "."                                   { \p s -> lexOut p LDot }
  "="                                   { \p s -> lexOut p LEqual }
  "\"                                   { \p s -> lexOut p LLambda }
  ";"                                   { \p s -> lexOut p LSemiColon }

  "let"                                 { \p s -> lexOut p LLet }
  "in"                                  { \p s -> lexOut p LIn }
  "fix"                                 { \p s -> lexOut p LFix }
  "unit"                                { \p s -> lexOut p LUnit }
  "S"                                   { \p s -> lexOut p LSucc }
  "natCase"                             { \p s -> lexOut p LNatCase }

  "_"                                   { \p s -> lexOut p LUnderscore }

  @str                                  { \p s -> lexOut p (LSym s) }
  @num                                  { \p s -> lexOut p (LNum $ read s) }

{
data Token = LLParen
           | LRParen
           | LDot
           | LEqual
           | LLambda
           | LSemiColon
           | LUnderscore
           | LLet
           | LIn
           | LFix
           | LUnit
           | LSucc
           | LNatCase
           | LSym String
           | LNum Int
           deriving (Eq, Show)

data LexOut = LexOut { offset :: Int
                     , line :: Int
                     , column :: Int
                     , getToken :: Token }
              deriving (Eq, Show)

lexOut (AlexPn offset line col) tok = LexOut offset line col tok

scan = alexScanTokens
}
