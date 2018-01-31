module SynthDefs.C7 where

import           Functions
import           Sound.SC3.UGen             (Rate (AR, KR), UGen, mce)
import           Sound.SC3.UGen.Bindings.DB hiding (freeVerb, gVerb, hpf,
                                             limiter, lpf)
import           SynthDefs.Types
import           UGens

--
-- SynthDefs for c7.hs
--

c7t :: Num b => Buf -> Pointer -> FreqScale -> WindowSize -> Db -> Attack -> Release -> IO b
c7t buffer pointer freqScale windowSize db attack release =
  sd "c7t"
  $ o (k "a" db)
  $ env (k "g" 1) (k "at" attack) (k "rl" release)
  $ gVerb 0.5 1.0 1.0 0.5 15 1 0.7 0.5 300
  $ warp1 2 (k "b" buffer) pointerOsc  (k "fs" freqScale) (k "ws" windowSize) (-1) (k "ol" 1) 0.0 4
  where
    pointerOsc = 0.1 * sinOsc KR (k "p" pointer) 0
