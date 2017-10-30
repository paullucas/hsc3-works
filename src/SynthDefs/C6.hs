module SynthDefs.C6 where

import  Functions
import  SynthDefs.Types
import  Sound.SC3.UGen             (Rate (AR, KR), UGen, mce)
import  Sound.SC3.UGen.Bindings.DB hiding (freeVerb, gVerb, hpf, limiter, lpf)
import  UGens

--
-- SynthDefs for c6.hs
--

c6s :: Num b => Freq -> Db -> Release -> IO b
c6s frequency db release =
  sd "c6s"
  $ o (k "a" db)
  $ env (k "g" 1) 15 (k "rl" release)
  $ limiter 0.8 0.001
  $ sinOsc AR (mce [freq, freq + 1]) 1
  where
    freq = k "f" frequency

c6w :: Buf -> Pointer -> Db -> FreqScale -> WindowSize -> LPF -> HPF -> Attack -> Release -> IO Int
c6w buffer pointer db freqScale windowSize lpF hpF attack release =
  sd "c6w"
  $ o (k "a" db)
  $ env (k "g" 1) (k "at" attack) (k "rl" release)
  $ hpf (k "hff" hpF)
  $ lpf (k "lff" lpF)
  $ warp1 2 (k "b" buffer) (k "p" pointer) (k "fs" freqScale) (k "ws" windowSize) (-1) (k "ol" 1) 0.0 4

c6wr :: Buf -> Pointer -> Db -> FreqScale -> WindowSize -> LPF -> HPF -> IO Int
c6wr buffer pointer db freqScale windowSize lpF hpF =
  sd "c6wr"
  $ o (k "a" db)
  $ env (k "g" 1) 15 40
  $ freeVerb 1 1 1
  $ hpf (k "hff" hpF)
  $ lpf (k "lff" lpF)
  $ warp1 2 (k "b" buffer) (k "p" pointer) (k "fs" freqScale) (k "ws" windowSize) (-1) (k "ol" 1) 0.0 4
