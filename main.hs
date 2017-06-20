module Main where
    
import qualified Data.Map as M
import Lexer (tokenize)
import Parser (parse)
import Evaluator (evaluate)

main = do 
    loop (M.fromList [("pi", pi)])

loop symTab = do
    str <- getLine
    if null str
    then 
        return ()
    else
        let toks = tokenize str
            tree = parse toks
        in case evaluate tree symTab of 
                Left msg -> do 
                    putStrLn $ "Error: "  ++ msg
                    loop symTab
                Right (val, symTab') -> do
                    print val
                    loop symTab'