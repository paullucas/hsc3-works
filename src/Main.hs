module Main where

import System.Process
import Functions
import SynthDefs
import UGens

main :: IO ()
main = do
    system "scsynth -u 57110 &"
    system "sleep 5"
    system "/bin/bash -c \"[ \"$(uname -s)\" == \"Linux\" ] && jack_connect SuperCollider:out_1 system:playback_1\""
    system "/bin/bash -c \"[ \"$(uname -s)\" == \"Linux\" ] && jack_connect SuperCollider:out_2 system:playback_2\""
    putStrLn "Ok!"
