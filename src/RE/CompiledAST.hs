{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveFunctor #-}
module RE.CompiledAST where

import Control.Monad.Free
import Control.Monad.Free.TH
import qualified Data.Map as M

-- A list of instructions to execute, with labels
-- taking values from `a'
data CompiledAST a next
    = Label a next
    | Character Char next
    | Jump a next
    | Split a a next
    | Match
    deriving (Functor, Show)

$(makeFree ''CompiledAST)

type FCompiledAST a = Free (CompiledAST a) ()
newtype JumpTable a = JumpTable { getJumpTable :: M.Map a (FCompiledAST a) }
    deriving (Show)

buildJumpTable :: Ord a => FCompiledAST a -> JumpTable a
buildJumpTable = JumpTable . buildJumpTable' M.empty

buildJumpTable' :: Ord a => M.Map a (FCompiledAST a) -> FCompiledAST a -> M.Map a (FCompiledAST a)
buildJumpTable' m (Pure _) = m
buildJumpTable' m (Free x) = case x of
    Label a next -> buildJumpTable' (M.insert a next m) next
    Character _ next -> buildJumpTable' m next
    Jump _ next      -> buildJumpTable' m next
    Split _ _ next   -> buildJumpTable' m next
    Match            -> m
