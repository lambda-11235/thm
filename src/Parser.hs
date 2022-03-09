-- TTyped: A dependently typed programming language.
-- Copyright (C) 2018  Taran Lynn
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.


module Parser where

import CST
import Lexer

import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Prim


type Parser = Parsec [LexOut] ()


match :: Token -> Parser ()
match tok = tokenPrim (show . getToken) pos (match' . getToken)
  where
    match' x = if x == tok then Just () else Nothing

sym :: Parser String
sym = tokenPrim (show . getToken) pos (match' . getToken)
  where
    match' (LSym name) = Just name
    match' _ = Nothing

pos :: (SourcePos -> LexOut -> [LexOut] -> SourcePos)
pos oldPos (LexOut _ line col _) _ = newPos (sourceName oldPos) line col


-- * Grammar

-- | A top level entry in the REPL.
topREPL :: Parser Expr
topREPL = expr <* eof

pfile :: Parser [Statement]
pfile = many statement <* eof


statement :: Parser Statement
statement = (TypeDefS <$> typeDef) <|> (FuncDefS <$> funcdef)


typeDef :: Parser TypeDef
typeDef =
  do match LType
     tname <- sym
     tvars <- many (match LTick *> sym)
     match LLBracket
     cdefs <- many consDef
     match LRBracket
     return (TypeDef tname tvars cdefs)

consDef :: Parser ConsDef
consDef =
  do cname <- sym
     targs <- option [] (match LLParen *>
                         sepBy ptype (match LComma)
                         <* match LRParen)
     return (ConsDef cname targs)


ptype :: Parser Type
ptype = try funType <|> ptype1

funType =
  do t1 <- ptype1
     match LRightArrow
     t2 <- ptype
     return (FunType t1 t2)

ptype1 = try tycons <|> ptype2

tycons =
  do name <- sym
     args <- many ptype2
     return (TyCons name args)

ptype2 = tvar <|> tparen <|> tycons1
tvar = match LTick *> (TyVar <$> sym)
tparen = match LLParen *> ptype <* match LRParen
tycons1 =
  do name <- sym
     return (TyCons name [])


funcdef :: Parser FuncDef
funcdef = do fixed <- option False (match LFix *> return True)
             name <- sym
             vars <- many arg
             match LEqual
             e <- expr
             match LSemiColon
             return (FuncDef fixed name vars e)


expr :: Parser Expr
expr = plet <|> lambda <|> app


plet :: Parser Expr
plet = do match LLet
          fdefs <- many1 funcdef
          match LIn
          e <- expr
          return (Let fdefs e)

lambda :: Parser Expr
lambda = do match LLambda
            args <- many1 arg
            match LDot
            e <- expr
            return (Lambda args e)

app :: Parser Expr
app = do e <- atom
         es <- many atom
         return (appify e es)
  where
    appify e [] = e
    appify e (e':es) = appify (App e e') es


atom :: Parser Expr
atom = (match LLParen *> expr <* match LRParen) <|> (fmap Var sym) <|> keyword

keyword :: Parser Expr
keyword = (match LFix *> return Fix) <|> pcase

pcase :: Parser Expr
pcase = do match LCase
           match LLBracket
           cons <- sym
           match LRBracket
           return (Case cons)

arg :: Parser (Maybe String)
arg = (fmap Just sym) <|> (match LUnderscore *> return Nothing)
