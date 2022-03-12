{
module Lexer (Token (..), LexOut (..), scan) where
}

%wrapper "posn"

@str = [a-zA-Z][a-zA-Z0-9]*

tokens :-

  $white+                               ;
  "#".*                                 ;

  "("                                   { \p s -> lexOut p LLParen }
  ")"                                   { \p s -> lexOut p LRParen }
  "["                                   { \p s -> lexOut p LLBracket }
  "]"                                   { \p s -> lexOut p LRBracket }

  ","                                   { \p s -> lexOut p LComma }
  "."                                   { \p s -> lexOut p LDot }
  "="                                   { \p s -> lexOut p LEqual }

  "\"                                   { \p s -> lexOut p LLambda }
  "λ"                                   { \p s -> lexOut p LLambda }
  
  "->"                                  { \p s -> lexOut p LRightArrow }
  ";"                                   { \p s -> lexOut p LSemiColon }
  "'"                                   { \p s -> lexOut p LTick }
  "_"                                   { \p s -> lexOut p LUnderscore }

  "case"                                { \p s -> lexOut p LCase }
  "fix"                                 { \p s -> lexOut p LFix }
  "in"                                  { \p s -> lexOut p LIn }
  "let"                                 { \p s -> lexOut p LLet }
  "type"                                { \p s -> lexOut p LType }

  @str                                  { \p s -> lexOut p (LSym s) }

{
data Token = LLParen
           | LRParen
           | LLBracket
           | LRBracket
           | LComma
           | LDot
           | LEqual
           | LLambda
           | LRightArrow
           | LSemiColon
           | LTick
           | LUnderscore
           | LCase
           | LFix
           | LIn
           | LLet
           | LType
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
