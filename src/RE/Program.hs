{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveFunctor #-}
module RE.Program (Program(..), buildProgram, compactTable, extendedTable) where

import Control.Monad.Free
import Data.Maybe
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Map as M

import RE.Insn

data Program a = Program {
      insns     :: [InsnF String ()]
    , jumpTable :: M.Map String a
 } deriving (Show, Eq)

$(deriveJSON defaultOptions ''Program)

type TableBuilder a = [InsnF String ()] -> M.Map String a

buildProgram :: TableBuilder a -> [InsnF String ()] -> Program a
buildProgram tableBuilder insns = Program insns $ tableBuilder insns

compactTable :: TableBuilder Int
compactTable = compactTable' 0 M.empty
    where compactTable' _ m []       = m
          compactTable' n m (x:next) = case x of
              Label a _     -> compactTable' (n + 1) (M.insert a n m) next
              Character _ _ -> compactTable' (n + 1) m next
              Jump _ _      -> compactTable' (n + 1) m next
              Split _ _ _   -> compactTable' (n + 1) m next
              Match         -> m

extendedTable :: TableBuilder [InsnF String ()]
extendedTable = extendedTable' M.empty
    where extendedTable' m []    = m
          extendedTable' m (h:t) = case h of
            Label a _     -> extendedTable' (M.insert a t m) t
            Character _ _ -> extendedTable' m t
            Jump _ _      -> extendedTable' m t
            Split _ _ _   -> extendedTable' m t
            Match         -> m
