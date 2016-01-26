{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveFunctor, DeriveGeneric #-}
module RE.Program (Program(..), buildProgram, doJump, extractInsn) where

import Control.Monad.Free
import Data.Maybe
import Data.Aeson
import qualified Data.Map as M
import GHC.Generics

import RE.Insn

data Program a = Program {
      insns     :: InsnList a
    , jumpTable :: M.Map a (InsnList a)
 } deriving (Show, Generic)

instance ToJSON a => ToJSON (Program a) where
    toEncoding = genericToEncoding defaultOptions

buildProgram :: Ord a => InsnList a -> Program a
buildProgram insns = Program insns $ buildJumpTable insns

buildJumpTable :: Ord a => InsnList a -> M.Map a (InsnList a)
buildJumpTable = buildJumpTable' M.empty
    where buildJumpTable' m (Pure _) = m
          buildJumpTable' m (Free x) = case x of
              Label a next -> buildJumpTable' (M.insert a next m) next
              Character _ next -> buildJumpTable' m next
              Jump _ next      -> buildJumpTable' m next
              Split _ _ next   -> buildJumpTable' m next
              Match            -> m

-- Given a label, computes a new program with the label jumped to
doJump :: Ord a => Program a -> a -> Program a
doJump (Program _ tab) idx = Program (fromJust $ M.lookup idx tab) tab

-- Given a program, try to extract 1 instruction 
extractInsn :: Program a -> Maybe (InsnF a (), Program a)
extractInsn (Program (Pure _) _)   = Nothing
extractInsn (Program (Free x) tab) = Just $ case x of
    Label a next         -> (Label a (), Program next tab)
    Character c next     -> (Character c (), Program next tab)
    Jump idx next        -> (Jump idx (), Program next tab)
    Split idx1 idx2 next -> (Split idx1 idx2 (), Program next tab)
    Match                -> (Match, Program (Pure ()) tab)
