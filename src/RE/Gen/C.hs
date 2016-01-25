module RE.Gen.C (cGen) where

import Text.Printf

import RE.Gen
import RE.Insn
import RE.Program

cGen :: Gen Int String
cGen = Gen generateCode

generateCode :: Program Int -> String
generateCode prgm = prologue ++ generateInsns prgm ++ epilogue
    where prologue = "int match0(char *s, int label) {\n    switch (label) {\n"
          epilogue = "    }\n}\n\nint match(char *s) {\n    return match0(s, 0);\n}\n\n"

generateInsns :: Program Int -> String
generateInsns prgm = case extractInsn prgm of
    Nothing            -> ""
    Just (insn, prgm') -> generateInsn insn ++ generateInsns prgm'

generateInsn :: InsnF Int () -> String
generateInsn (Label idx _)       = printf "        case %d:\n" idx
generateInsn (Character c _)     = printf "            if (*s++ != '%c') {\n                return 0;\n            }\n" c
generateInsn (Jump idx _)        = printf "            return match(s, %d);\n" idx
generateInsn (Split idx1 idx2 _) = printf "            return match(s, %d) || match(s, %d);\n" idx1 idx2
generateInsn Match               = printf "            return 1;\n"
