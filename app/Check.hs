module Check (checks, getCheck, checkMessage) where

import Opts

data Check
    = NoNonOptions
    | PatternRequired
    deriving (Enum)

checks :: [Check]
checks = [NoNonOptions ..]

getCheck :: ([Flag], [String]) -> Check -> Bool
getCheck (_, non)  NoNonOptions    = not $ null non
getCheck (opts, _) PatternRequired = not $ any isPattern opts

checkMessage :: Check -> String
checkMessage NoNonOptions    = "unknown argument passed\n"
checkMessage PatternRequired = "pattern is required\n"
