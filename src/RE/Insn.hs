{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveFunctor, DeriveGeneric, FlexibleInstances, DeriveFoldable #-}
module RE.Insn where

import Control.Monad.Free
import Control.Monad.Free.TH
import Data.Aeson
import GHC.Generics

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
    deriving (Functor, Generic, Show, Foldable)

instance (ToJSON a, ToJSON next) => ToJSON (InsnF a next) where
    toEncoding = genericToEncoding defaultOptions

$(makeFree ''InsnF)

type InsnList a = Free (InsnF a) ()

instance (Functor f, Foldable f, ToJSON (f ())) => ToJSON (Free f a) where
    toJSON = toJSON . listify

