module SynthDefs where

import Functions
import UGens
import Sound.SC3.UGen (UGen, Rate (AR, KR), mce)
import Sound.SC3.UGen.Bindings.DB hiding (lpf, hpf, limiter, freeVerb, gVerb)

--
-- SynthDefs for c6.hs
--

c6s :: Num b => Double -> Double -> Double -> IO b
c6s frequency db release =
  sd "c6s"
  $ o (k "a" db)
  $ env (k "g" 1) 15 (k "rl" release)
  $ limiter 0.8 0.001
  $ sinOsc AR (mce [freq, freq + 1]) 1
  where
    freq = k "f" frequency

c6w :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO Int
c6w buffer pointer db freqScale windowSize lpF hpF attack release =
  sd "c6w"
  $ o (k "a" db)
  $ env (k "g" 1) (k "at" attack) (k "rl" release)
  $ hpf (k "hff" hpF)
  $ lpf (k "lff" lpF)
  $ warp1 2 (k "b" buffer) (k "p" pointer) (k "fs" freqScale) (k "ws" windowSize) (-1) (k "ol" 1) 0.0 4

c6wr :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO Int
c6wr buffer pointer db freqScale windowSize lpF hpF =
  sd "c6wr"
  $ o (k "a" db)
  $ env (k "g" 1) 15 40
  $ freeVerb 1 1 1
  $ hpf (k "hff" hpF)
  $ lpf (k "lff" lpF)
  $ warp1 2 (k "b" buffer) (k "p" pointer) (k "fs" freqScale) (k "ws" windowSize) (-1) (k "ol" 1) 0.0 4
