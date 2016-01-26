module RE.Gen.C (cGen) where

import Text.Printf

import RE.Gen
import RE.Insn
import RE.Program

cGen :: Gen String String
cGen = Gen generateCode

generateCode :: Program String -> String
generateCode (Program insns _) = prologue ++ (insns >>= generateInsn) ++ epilogue
    where prologue = "int match0(char *s, int label) {\n    switch (label) {\n"
          epilogue = "    }\n}\n\nint match(char *s) {\n    return match0(s, 0);\n}\n\n"

generateInsn :: InsnF String () -> String
generateInsn (Label idx _)       = printf "        case %s:\n" idx
generateInsn (Character c _)     = printf "            if (*s++ != '%c') {\n                return 0;\n            }\n" c
generateInsn (Jump idx _)        = printf "            return match0(s, %s);\n" idx
generateInsn (Split idx1 idx2 _) = printf "            return match0(s, %s) || match0(s, %s);\n" idx1 idx2
generateInsn Match               = printf "            return 1;\n"
