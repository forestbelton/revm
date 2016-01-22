module RE.Interpret where

import Control.Monad.Free
import qualified Data.Map as M
import Data.Maybe

import RE.CompiledAST

interpret :: Ord a => String -> FCompiledAST a -> JumpTable a -> Bool
interpret _ (Pure _) _ = False
interpret s (Free x) jt@(JumpTable tab) = case x of
    Label _ next -> interpret s next jt
    Character c next -> case s of
        [] -> False
        (h:t) -> if h == c
            then interpret t next jt
            else False
    Jump idx next -> interpret s (fromJust $ M.lookup idx tab) jt
    Split idx1 idx2 next -> let branch1 = interpret s (fromJust $ M.lookup idx1 tab) jt
                                branch2 = interpret s (fromJust $ M.lookup idx2 tab) jt in
                            branch1 || branch2
    Match -> True
