module SynthDefs.C8 where

import           Functions
import           Sound.SC3.UGen             (Rate (KR))
import           Sound.SC3.UGen.Bindings.DB hiding (freeVerb, hpf, lpf)
import           SynthDefs.Types
import           UGens

--
-- SynthDefs for c8.hs
--

c8p :: Buf -> SRate -> Trig -> SPos -> Db -> LPF -> HPF -> Attack -> Release -> IO Int
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

c8w :: Buf -> Pointer -> Db -> FreqScale -> WindowSize -> LPF -> HPF -> IO Int
c8w buffer pointer db freqScale windowSize lpF hpF =
  sd "c8w"
  $ o (k "a" db)
  $ env (k "g" 1) 20 40
  $ freeVerb 1 1 1
  $ hpf (k "hff" hpF)
  $ lpf (k "lff" lpF)
  $ warp1 2 (k "b" buffer) (k "p" pointer) (k "fs" freqScale) (k "ws" windowSize) (-1) (k "ol" 1) 0.0 4
