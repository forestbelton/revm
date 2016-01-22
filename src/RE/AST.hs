module RE.AST where

data UnOp
    = Many
    | Many1
    | Optional
    deriving (Show)

data BinOp
    = Sequence
    | Alternative
    deriving (Show)

data AST
    = Literal Char
    | Unary AST UnOp
    | Binary BinOp AST AST
    deriving (Show)
