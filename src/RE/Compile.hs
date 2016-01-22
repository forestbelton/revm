{-# LANGUAGE FlexibleContexts #-}
module RE.Compile where

import Control.Monad.Free
import Control.Monad.State

import RE.AST
import RE.CompiledAST

nextLabel :: Monad m => StateT Int m Int
nextLabel = do
    next <- get
    modify (+ 1)
    return next

nextLabels :: Monad m => Int -> StateT Int m [Int]
nextLabels 0 = return []
nextLabels n = (:) <$> nextLabel <*> nextLabels (n - 1)

compile :: AST -> Free (CompiledAST Int) ()
compile ast = do
    evalStateT (compile' ast) 0
    match

compile' :: MonadFree (CompiledAST Int) m => AST -> StateT Int m ()
compile' (Literal c) = character c
compile' (Binary Sequence e1 e2) = do
    compile' e1
    compile' e2
compile' (Binary Alternative e1 e2) = do
    [l1, l2, l3] <- nextLabels 3
    split l1 l2
    label l1
    compile' e1
    jump l3
    label l2
    compile' e2
    label l3
compile' (Unary e Many) = do
    [l1, l2, l3] <- nextLabels 3
    label l1
    split l2 l3
    label l2
    compile' e
    jump l1
    label l3
compile' (Unary e Many1) = do
    [l1, l2] <- nextLabels 2
    label l1
    compile' e
    split l1 l2
    label l2
