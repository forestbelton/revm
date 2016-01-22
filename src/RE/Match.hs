module RE.Match where

import RE.Compile
import RE.CompiledAST
import RE.Interpret
import RE.Parse

newtype RegExp = RegExp String

match :: RegExp -> String -> Bool
match (RegExp re) s = interpret s compiled jt 
    where compiled = compile $ parse re
          jt = buildJumpTable compiled
