module RE.Gen where

import RE.Program

newtype Gen a s = Gen { runGenerator :: Program a -> s }
