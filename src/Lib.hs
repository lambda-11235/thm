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
import AST
import qualified TypeCheck as TC
import qualified TypeDef as TD
import qualified Eval as E

loadFile :: String -> IO [Statement]
loadFile fname =
  do contents <- readFile fname
     let toks = scan contents
     case parse pfile fname toks of
       Left err -> fail (show err)
       Right ast -> return (map statementFromCST ast)

loadFiles :: [String] -> IO [Statement]
loadFiles fnames = fmap concat (mapM loadFile fnames)

processFiles :: Bool -> [String] -> IO (TD.Bindings, TC.Context, E.Bindings)
processFiles pprint fnames =
  do states <- loadFiles fnames
     procFilesSub states TD.empty TC.empty M.empty
  where
    procFilesSub :: [Statement] -> TD.Bindings -> TC.Context
                 -> E.Bindings -> IO (TD.Bindings, TC.Context, E.Bindings)
    procFilesSub [] tdbs tctx dbs = return (tdbs, tctx, dbs)
    procFilesSub ((TypeDefS tdef):states) tdbs tctx dbs =
      case TD.updateBindings tdef tdbs of
        Left err -> fail ("Type Definition Error: " ++ err)
        Right tdbs' -> procFilesSub states tdbs' tctx dbs
    procFilesSub ((FuncDefS (FuncDef name e)):state) tdbs tctx dbs =
      do t <- checkExpr e tdbs tctx
         x <- evalExpr pprint e tdbs dbs
         when pprint (do putStrLn (name ++ " : " ++ ppType t)
                         putStrLn (name ++ " = " ++ E.ppData x)
                         when (not $ null state) (putStrLn ""))
         let tctx' = TC.insertLet name t tctx
         let dbs' = M.insert name x dbs
         procFilesSub state tdbs tctx' dbs'


repl :: (TD.Bindings, TC.Context, E.Bindings) -> IO ()
repl bindings@(tdbs, tctx, dbs) =
  do putStr "λ> "
     hFlush stdout

     eof <- hIsEOF stdin
     if eof then return () else
       do str <- getLine
          let toks = scan str
          case parse topREPL "REPL" toks of
            Left err -> putStrLn (show err) >> repl bindings
            Right ast ->
              do let e = exprFromCST ast
                 case TC.runEnv (TC.checkExpr e tdbs tctx) of
                   Left err -> putStrLn ("Type Error: " ++ err)
                   Right t ->
                     case (E.evalExpr tdbs dbs e) >>= (E.force tdbs) of
                       Left err -> putStrLn ("Evaluation Error: " ++ err)
                       Right x -> putStrLn (E.ppData x ++ " : " ++ ppType t)
                 repl bindings


checkExpr :: Expr -> TD.Bindings -> TC.Context -> IO Type
checkExpr e tdbs tctx =
  case TC.runEnv (TC.checkExpr e tdbs tctx) of
    Left err -> fail ("Type Error: " ++ err)
    Right x -> return x


evalExpr :: Bool -> Expr -> TD.Bindings -> E.Bindings -> IO E.Data
evalExpr force e tdbs dbs =
  let e1 = E.evalExpr tdbs dbs e
      e2 = if force then e1 >>= (E.force tdbs) else e1
  in case e2 of
       Left err -> fail ("Evaluation Error: " ++ err)
       Right x -> return x
