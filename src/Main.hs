{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import           Functions
import           SynthDefs.C10
import           SynthDefs.C6
import           SynthDefs.C7
import           SynthDefs.C8
import           SynthDefs.Types
import           UGens

main :: IO ()
main = boot
