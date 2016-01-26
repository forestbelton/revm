module RE.Match where

import RE.Compile
import RE.Insn
import RE.Interpret
import RE.Parse
import RE.Program

newtype RegExp = RegExp { extractRegExp :: String }

compileRE :: RegExp -> Program [InsnF String ()]
compileRE = buildProgram extendedTable . compile . parse . extractRegExp

match :: RegExp -> String -> Bool
match re s = interpret s (compileRE re)
