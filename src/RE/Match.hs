module RE.Match where

import RE.Compile
import RE.Program
import RE.Interpret
import RE.Parse

newtype RegExp = RegExp { extractRegExp :: String }

compileRE :: RegExp -> Program String
compileRE = buildProgram . compile . parse . extractRegExp

match :: RegExp -> String -> Bool
match re s = interpret s (compileRE re)
