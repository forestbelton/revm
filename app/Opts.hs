module Opts where

import System.Console.GetOpt

data Flag
    = Language String
    | Pattern String
    deriving (Show)

isLanguage :: Flag -> Bool
isLanguage (Language _) = True
isLanguage _            = False

isPattern :: Flag -> Bool
isPattern (Pattern _) = True
isPattern _           = False

header :: String
header = "Usage: revm [OPTION...]"

options :: [OptDescr Flag]
options =
    [ Option [] ["language"] (ReqArg Language "LANG") "language LANG"
    , Option [] ["pattern"]  (ReqArg Pattern "PAT") "pattern PAT"
    ]

programOpts :: [String] -> IO ([Flag], [String])
programOpts argv = case getOpt RequireOrder options argv of
    (o, n, [])   -> return (o, n)
    (_, _, errs) -> printUsage $ concat errs

printUsage :: String -> IO a
printUsage errs = ioError $ userError $ errs ++ usageInfo header options
