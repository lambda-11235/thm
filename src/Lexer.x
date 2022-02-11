{
module Lexer (Token (..), LexOut (..), scan) where
}

%wrapper "posn"

@char = [a-zA-Z0-9]

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
  "Z"                                   { \p s -> lexOut p LZero }
  "S"                                   { \p s -> lexOut p LSucc }
  "natCase"                             { \p s -> lexOut p LNatCase }

  "_"                                   { \p s -> lexOut p LUnderscore }

  @char+                                { \p s -> lexOut p (LSym s) }

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
           | LZero
           | LSucc
           | LNatCase
           | LSym String
           deriving (Eq, Show)

data LexOut = LexOut { offset :: Int
                     , line :: Int
                     , column :: Int
                     , getToken :: Token }
              deriving (Eq, Show)

lexOut (AlexPn offset line col) tok = LexOut offset line col tok

scan = alexScanTokens
}
