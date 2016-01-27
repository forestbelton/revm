module Lib (compileToJSON) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B

import RE.Compile
import RE.Parse
import RE.Program

compileToJSON :: String -> B.ByteString
compileToJSON = encode . buildProgram compactTable . compile . parse
