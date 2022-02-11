
import Data.Either
import Text.Parsec.Prim

import CST
import Lexer
import Parser

main =
  do s <- readFile  "../lib/nat.thm"
     let p = fromRight [] $ parse pfile "" (scan s)
     mapM_ putStrLn $ map (ppFuncDef . fdefFromAST) p
