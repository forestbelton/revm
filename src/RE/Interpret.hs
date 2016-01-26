module RE.Interpret (interpret) where

import Control.Monad.Free
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe

import RE.Insn
import RE.Program

doJump :: Program [InsnF String ()] -> String -> Program [InsnF String ()]
doJump (Program _ tab) idx = Program (fromJust $ M.lookup idx tab) tab

interpret :: String -> Program [InsnF String ()] -> Bool
interpret s (Program [] _)           = False
interpret s prgm@(Program (h:next) tab) = case h of
    Label _ _         -> interpret s $ Program next tab
    Character c _     -> case s of
        []    -> False
        (h:t) -> if c == h
            then interpret t $ Program next tab
            else False
    Jump idx _        -> interpret s $ doJump prgm idx
    Split idx1 idx2 _ -> let branch1 = interpret s $ doJump prgm idx1
                             branch2 = interpret s $ doJump prgm idx2
                         in branch1 || branch2
    Match             -> True
