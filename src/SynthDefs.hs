module SynthDefs where

import  Functions
import  Sound.SC3.UGen             (Rate (AR, KR), UGen, mce)
import  Sound.SC3.UGen.Bindings.DB hiding (freeVerb, gVerb, hpf,
                                    limiter, lpf)
import  UGens

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

--
-- SynthDefs for c7.hs
--

c7t :: Num b => Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO b
c7t buffer pointer freqScale windowSize db attack release =
  sd "c7t"
  $ o (k "a" db)
  $ env (k "g" 1) (k "at" attack) (k "rl" release)
  $ gVerb 0.5 1.0 1.0 0.5 15 1 0.7 0.5 300
  $ warp1 2 (k "b" buffer) pointerOsc  (k "fs" freqScale) (k "ws" windowSize) (-1) (k "ol" 1) 0.0 4
  where
    pointerOsc = 0.1 * sinOsc KR (k "p" pointer) 0

--
-- SynthDefs for c8.hs
--

c8p :: Num b => Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO b
c8p buffer rate trigger startPos db lpF hpF attack release =
  sd "c8p"
  $ o (k "a" db)
  $ env (k "g" 1) (k "at" attack) (k "rl" release)
  $ freeVerb 1 1 1
  $ hpf (k "hff" hpF)
  $ lpf (k "lff" lpF)
  $ pbuf (k "b" buffer) (k "r" rate) impTrig (k "p" startPos)
    where
      impTrig = impulse KR (k "t" trigger) 0

c8w :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO Int
c8w buffer pointer db freqScale windowSize lpF hpF =
  sd "c8w"
  $ o (k "a" db)
  $ env (k "g" 1) 20 40
  $ freeVerb 1 1 1
  $ hpf (k "hff" hpF)
  $ lpf (k "lff" lpF)
  $ warp1 2 (k "b" buffer) (k "p" pointer) (k "fs" freqScale) (k "ws" windowSize) (-1) (k "ol" 1) 0.0 4
