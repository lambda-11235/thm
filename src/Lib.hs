module Lib
    ( checkFiles
    ) where

import qualified Data.Map as M
import Text.Parsec (parse)

import Lexer
import Parser
import CST
import TypeCheck

loadFile :: String -> IO [FuncDef]
loadFile fname =
  do contents <- readFile fname
     let toks = scan contents
     case parse pfile fname toks of
       Left err -> fail (show err)
       Right ast -> return (map fdefFromAST ast)

loadFiles :: [String] -> IO [FuncDef]
loadFiles fnames = fmap concat (mapM loadFile fnames)

checkFiles :: [String] -> IO ()
checkFiles fnames =
  do fdefs <- loadFiles fnames
     checkFileSub fdefs M.empty
  where
    checkFileSub [] _ = return ()
    checkFileSub ((FuncDef name e):fdefs) bs =
      case runContext (checkExpr e bs) of
        Left err -> fail err
        Right (uitvs, t) ->
          do putStrLn (name ++ " : " ++ ppType t)
             checkFileSub fdefs (M.insert name (uitvs, t) bs)
