module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import Lib

main :: IO ()
main = do
    inp <- getLine
    B.putStrLn $ compileToJSON inp
