module Lib
    ( processFiles
    , repl
    ) where

import Control.Monad (when)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec (parse)
import System.IO

import Lexer
import Parser
import CST
import qualified TypeCheck as TC
import qualified Eval as E

loadFile :: String -> IO [FuncDef]
loadFile fname =
  do contents <- readFile fname
     let toks = scan contents
     case parse pfile fname toks of
       Left err -> fail (show err)
       Right ast -> return (map fdefFromAST ast)

loadFiles :: [String] -> IO [FuncDef]
loadFiles fnames = fmap concat (mapM loadFile fnames)

processFiles :: Bool -> [String] -> IO (TC.Bindings, E.Bindings)
processFiles pprint fnames =
  do fdefs <- loadFiles fnames
     procFilesSub fdefs M.empty M.empty
  where
    procFilesSub :: [FuncDef] -> TC.Bindings -> E.Bindings -> IO (TC.Bindings, E.Bindings)
    procFilesSub [] tbs dbs = return (tbs, dbs)
    procFilesSub ((FuncDef name e):fdefs) tbs dbs =
      case TC.runContext (TC.checkExpr e tbs) of
        Left err -> fail ("Type Error: " ++ err)
        Right (uitvs, t) ->
          case E.evalExpr dbs e of
            Left err -> fail ("Evaluation Error: " ++ err)
            Right x ->
              do when pprint (do putStrLn (name ++ " : " ++ ppType t)
                                 putStrLn (name ++ " = " ++ E.ppData x)
                                 when (not $ null fdefs) (putStrLn ""))
                 let tbs' = TC.insert name uitvs t tbs
                 let dbs' = M.insert name x dbs
                 procFilesSub fdefs tbs' dbs'

repl :: (TC.Bindings, E.Bindings) -> IO ()
repl bindings@(tbs, dbs) =
  do putStr "λ> "
     hFlush stdout

     eof <- hIsEOF stdin
     if eof then return () else
       do str <- getLine
          let toks = scan str
          case parse topREPL "REPL" toks of
            Left err -> putStrLn (show err) >> repl bindings
            Right ast ->
              do let e = exprFromAST ast
                 case TC.runContext (TC.checkExpr e tbs) of
                   Left err -> putStrLn ("Type Error: " ++ err) >> repl bindings
                   Right (uitvs, t) ->
                     case E.evalExpr dbs e of
                       Left err -> putStrLn ("Evaluation Error: " ++ err) >> repl bindings
                       Right x ->
                         do putStrLn (E.ppData x ++ " : " ++ ppType t)
                            repl bindings
