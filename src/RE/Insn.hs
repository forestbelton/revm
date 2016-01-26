{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveFunctor, FlexibleInstances, DeriveFoldable #-}
module RE.Insn where

import Control.Monad.Free
import Control.Monad.Free.TH
import Data.Aeson
import Data.Aeson.TH

listify :: (Functor f, Foldable f) => Free f a -> [f ()]
listify (Pure _) = []
listify (Free x) = fmap (const ()) x : foldMap listify x

-- A list of instructions to execute, with labels
-- taking values from `a'
data InsnF a next
    = Label a next
    | Character Char next
    | Jump a next
    | Split a a next
    | Match
    deriving (Functor, Show, Foldable, Eq)

$(deriveJSON defaultOptions ''InsnF)
$(makeFree ''InsnF)
