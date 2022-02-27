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
import qualified TypeDef as TD
import qualified Eval as E

loadFile :: String -> IO [Statement]
loadFile fname =
  do contents <- readFile fname
     let toks = scan contents
     case parse pfile fname toks of
       Left err -> fail (show err)
       Right ast -> return (map statementFromAST ast)

loadFiles :: [String] -> IO [Statement]
loadFiles fnames = fmap concat (mapM loadFile fnames)

processFiles :: Bool -> [String] -> IO (TD.Bindings, TC.Bindings, E.Bindings)
processFiles pprint fnames =
  do states <- loadFiles fnames
     procFilesSub states TD.empty M.empty M.empty
  where
    procFilesSub :: [Statement] -> TD.Bindings -> TC.Bindings
                 -> E.Bindings -> IO (TD.Bindings, TC.Bindings, E.Bindings)
    procFilesSub [] tdbs tbs dbs = return (tdbs, tbs, dbs)
    procFilesSub ((TypeDefS tdef):states) tdbs tbs dbs =
      case TD.updateBindings tdef tdbs of
        Left err -> fail ("Type Definition Error: " ++ err)
        Right tdbs' -> procFilesSub states tdbs' tbs dbs
    procFilesSub ((FuncDefS (FuncDef name e)):state) tdbs tbs dbs =
      do (uitvs, t) <- checkExpr e tdbs tbs
         x <- evalExpr pprint e tdbs dbs
         when pprint (do putStrLn (name ++ " : " ++ ppType t)
                         putStrLn (name ++ " = " ++ E.ppData x)
                         when (not $ null state) (putStrLn ""))
         let tbs' = TC.insert name uitvs t tbs
         let dbs' = M.insert name x dbs
         procFilesSub state tdbs tbs' dbs'


repl :: (TD.Bindings, TC.Bindings, E.Bindings) -> IO ()
repl bindings@(tdbs, tbs, dbs) =
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
                 case TC.runContext (TC.checkExpr e tdbs tbs) of
                   Left err -> putStrLn ("Type Error: " ++ err)
                   Right (_, t) ->
                     case (E.evalExpr tdbs dbs e) >>= (E.force tdbs) of
                       Left err -> putStrLn ("Evaluation Error: " ++ err)
                       Right x -> putStrLn (E.ppData x ++ " : " ++ ppType t)
                 repl bindings


checkExpr :: Expr -> TD.Bindings -> TC.Bindings -> IO (TC.UnInstTyVars, Type)
checkExpr e tdbs tbs =
  case TC.runContext (TC.checkExpr e tdbs tbs) of
    Left err -> fail ("Type Error: " ++ err)
    Right x -> return x


evalExpr :: Bool -> Expr -> TD.Bindings -> E.Bindings -> IO E.Data
evalExpr force e tdbs dbs =
  let e1 = E.evalExpr tdbs dbs e
      e2 = if force then e1 >>= (E.force tdbs) else e1
  in case e2 of
       Left err -> fail ("Evaluation Error: " ++ err)
       Right x -> return x
