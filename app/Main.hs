module Main where

import Data.Maybe
import Data.List
import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment
import System.Exit
import System.Process
import System.IO

import Lib
import Opts
import Check

produceAST :: [Flag] -> IO ()
produceAST flags = do
    pipeBinary lang $ compileToJSON pat
    where (Language lang) = fromMaybe (Language "echo") (find isLanguage flags)
          (Pattern pat)   = fromJust $ find isPattern flags

pipeBinary :: String -> B.ByteString -> IO a
pipeBinary name ast = do
    dir <- fromMaybe "./langs" <$> lookupEnv "REVM_ROOT"
    (stdin, stdout, stderr, procHandle) <- createProcess $ (shell $ dir ++ "/lang-" ++ name) { std_in = CreatePipe }
    B.hPutStrLn (fromJust stdin) ast

    exitCode <- waitForProcess procHandle
    exitWith exitCode

main :: IO ()
main = do
    result <- getArgs >>= programOpts
    let errs = filter (getCheck result) checks
    if not $ null errs
        then printUsage $ concatMap checkMessage errs
        else let (opts, _) = result
                 in produceAST opts
