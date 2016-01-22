{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveFunctor #-}
module RE.CompiledAST where

import Control.Monad.Free
import Control.Monad.Free.TH

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
