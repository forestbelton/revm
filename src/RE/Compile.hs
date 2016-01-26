{-# LANGUAGE FlexibleContexts #-}
module RE.Compile (compile) where

import Control.Monad.Free
import Control.Monad.State

import RE.AST
import RE.Insn

nextLabel :: Monad m => StateT Int m String
nextLabel = do
    next <- get
    modify (+ 1)
    return $ show next

nextLabels :: Monad m => Int -> StateT Int m [String]
nextLabels 0 = return []
nextLabels n = (:) <$> nextLabel <*> nextLabels (n - 1)

compile :: AST -> InsnList String
compile ast = do
    label "0"
    evalStateT (compile' ast) 1
    match

compile' :: MonadFree (InsnF String) m => AST -> StateT Int m ()
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
