module RE.Interpret where

import Control.Monad.Free
import qualified Data.Map as M
import Data.Maybe

import RE.Insn
import RE.Program

interpret :: Ord a => String -> Program a -> Bool
interpret s prgm = case extractInsn prgm of 
    Nothing            -> False
    Just (insn, prgm') -> interpretInsn prgm' s insn

interpretInsn :: Ord a => Program a -> String -> InsnF a () -> Bool
interpretInsn prgm s (Label _ _)         = interpret s prgm
interpretInsn prgm [] (Character _ _)    = False
interpretInsn prgm (h:t) (Character c _) = if h == c
    then interpret t prgm
    else False
interpretInsn prgm s (Jump idx _)        = interpret s (doJump prgm idx)
interpretInsn prgm s (Split idx1 idx2 _) = branch1 || branch2
    where branch1 = interpret s (doJump prgm idx1)
          branch2 = interpret s (doJump prgm idx2)
interpretInsn _ _ Match                  = True
