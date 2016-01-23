{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveFunctor #-}
module RE.Insn where

import Control.Monad.Free
import Control.Monad.Free.TH

-- A list of instructions to execute, with labels
-- taking values from `a'
data InsnF a next
    = Label a next
    | Character Char next
    | Jump a next
    | Split a a next
    | Match
    deriving (Functor, Show)

$(makeFree ''InsnF)

type InsnList a = Free (InsnF a) ()
