import Data.Char

data Token = TokOp Operator
           | TokAssign
           | TokLParen
           | TokRParen
           | TokIdent String
           | TokNum Double
           | TokEnd
    deriving (Show, Eq)

data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs)
    | elem c "+-*/" = TokOp (operator c) : tokenize cs
    | c == '=' = TokAssign : tokenize cs
    | c == '(' = TokLParen : tokenize cs
    | c == ')' = TokRParen : tokenize cs
    | isDigit c = number c cs
    | isAlpha c = identifier c cs
    | isSpace c = tokenize cs
    | otherwise = error $ "Cannot tokenize " ++ [c]

identifier c cs = let (str, cs') = span isAlphaNum cs in TokIdent (c:str) : tokenize cs'
number c cs = let (digs, cs') = span isDigit cs in TokNum (read (c:digs)) : tokenize cs'

data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String
    deriving Show

lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (t:ts) = t

accept :: [Token] -> [Token]
accept [] = error "No token to accept"
accept (t:ts) = ts

expression :: [Token] -> (Tree, [Token])
expression toks = 
    let (teTree, toks') = term toks
    in case lookAhead toks' of 
        (TokOp op) | elem op [Plus, Minus] ->
            let (exTree, toks'') = expression $ accept toks'
            in (SumNode op teTree exTree, toks'')
        TokAssign -> 
            case teTree of
                VarNode str -> 
                    let (exTree, toks'') = expression $ accept toks'
                    in (AssignNode str exTree, toks'')
                _ -> error "Only variables can be assigned to"
        _ -> (teTree, toks')

term :: [Token] -> (Tree, [Token])
term toks = 
    let (facTree, toks') = factor toks
    in case lookAhead toks' of
        (TokOp op) | elem op [Times, Div] -> 
            let (exTree, toks'') = term $ accept toks'
            in (ProdNode op facTree exTree, toks'')
        _ -> (facTree, toks')
        
factor :: [Token] -> (Tree, [Token])
factor toks = 
    case lookAhead toks of 
        (TokNum x) -> (NumNode x, accept toks)
        (TokIdent str) -> (VarNode str, accept toks)
        (TokOp op) | elem op [Plus, Minus] -> 
            let (facTree, toks') = factor $ accept toks  
            in (UnaryNode op facTree, toks')
        TokLParen -> 
            let (exTree, toks') = expression $ accept toks
            in case lookAhead toks' of 
                TokRParen -> (exTree, accept toks')
                _ -> error "No matching right parenthesis"
        _ -> error $ "Parse error on token: " ++ show toks

parse :: [Token] -> Tree
parse toks = let (tree, toks') = expression toks 
             in if null toks' 
                 then tree
                 else error $ "Leftover tokens: " ++ show toks'

main = print $ parse $ tokenize "(12 + 24) / x1"