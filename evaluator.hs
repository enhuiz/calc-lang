module Evaluator(evaluate) where
    
import qualified Data.Map as M

import Lexer (Token(..), Operator(..))
import Parser (Tree(..))

type SymTab = M.Map String Double

evaluate :: Tree -> SymTab -> Either String (Double, SymTab)
evaluate (SumNode op left right) symTab =
    case evaluate left symTab of 
        Left msg -> Left msg
        Right (lft, symTab') -> 
            case evaluate right symTab' of 
                Left msg -> Left msg
                Right (rgt, symTab'') ->
                    case op of 
                        Plus -> Right (lft + rgt, symTab'')
                        Minus -> Right (lft - rgt, symTab'')

evaluate (ProdNode op left right) symTab =
    case evaluate left symTab of 
        Left msg -> Left msg
        Right (lft, symTab') -> 
            case evaluate right symTab' of 
                Left msg -> Left msg
                Right (rgt, symTab'') ->
                    case op of 
                        Times -> Right (lft * rgt, symTab'') 
                        Div -> Right (lft / rgt, symTab'')

evaluate (UnaryNode op tree) symTab =
    case evaluate tree symTab of 
        Left msg -> Left msg
        Right (x, symTab') -> 
            case op of 
                Plus -> Right (x, symTab')
                Minus -> Right (-x, symTab')

evaluate (NumNode x) symTab = Right (x, symTab)

evaluate (VarNode str) symTab = 
    case lookUp str symTab of
        Left msg -> Left msg
        Right (v, symTab') -> Right (v, symTab')

evaluate (AssignNode str tree) symTab =
    case evaluate tree symTab of 
        Left msg -> Left msg
        Right (v, symTab') -> 
            case addSymbol str v symTab' of
            Left msg -> Left msg
            Right (_, symTab'') -> Right (v, symTab'')
                
lookUp :: String -> SymTab -> Either String (Double, SymTab)
lookUp str symTab =
    case M.lookup str symTab of 
        Just v -> Right (v, symTab)
        Nothing -> Left $ "Undefined variable " ++ str

addSymbol :: String -> Double -> SymTab -> Either String ((), SymTab)
addSymbol str val symTab = 
    let symTab' = M.insert str val symTab
    in Right ((), symTab')