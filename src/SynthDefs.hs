module SynthDefs where

import Functions
import UGens
import Sound.SC3.UGen
import Sound.SC3.UGen.Bindings

--
-- SynthDefs for c6.hs
--

c4sio :: Double -> Double -> Double -> IO Int
c4sio f a rl =
  sd "c4sio"
  $ amp ampLevel
  $ env gate 15 release
  $ lmtr
  $ sinOsc AR (mce [freq, freq + 1]) 1
  where
    freq     = k "f" f
    ampLevel = k "a" a
    release  = k "rl" rl
    gate     = k "g" 1

c5wr :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO Int
c5wr b p a fs ws lff hff =
  sd "c5wr"
  $ amp ampLevel
  $ env gate 15 40
  $ fvrb' 1 1 1
  $ hf highpassFreq
  $ lf lowpassFreq
  $ warp1 2 buffer pointer freqScale windowSize (-1) overlaps 0.0 4
  where
    buffer       = k "b" b
    pointer      = k "p" p
    ampLevel     = k "a" a
    freqScale    = k "fs" fs
    windowSize   = k "ws" ws
    lowpassFreq  = k "lff" lff
    highpassFreq = k "hff" hff
    overlaps     = k "ol" 1
    gate         = k "g" 1

c6w :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO Int
c6w b p a fs ws lff hff at rl =
  sd "c6w"
  $ amp ampLevel
  $ env gate attack release
  $ hf highpassFreq
  $ lf lowpassFreq
  $ warp1 2 buffer pointer freqScale windowSize (-1) overlaps 0.0 4
  where
    buffer       = k "b" b
    pointer      = k "p" p
    ampLevel     = k "a" a
    freqScale    = k "fs" fs
    windowSize   = k "ws" ws
    lowpassFreq  = k "lff" lff
    highpassFreq = k "hff" hff
    attack       = k "at" at
    release      = k "rl" rl
    overlaps     = k "ol" 1
    gate         = k "g" 1
