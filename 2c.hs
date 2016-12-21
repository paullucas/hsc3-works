-- https://8c6794b6.github.io/posts/2011-10-25_adhoc-update-of-ugen-parameters.html

{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Control.Monad
import Data.Data (Data, Typeable)
import Data.Generics (everywhere, mkT)
import Data.List (zipWith4)
import Control.Concurrent (forkIO, threadDelay)
import System.Random

import Sound.SC3
import Sound.OSC
import Data.Generics.Uniplate.Data (transform, transformM)

g = withSC3 (send (g_new [(1, AddToTail, 0)]))
k = withSC3 (send (n_free [1]))

s01 :: UGen
s01 =
  let o = control IR "out" 0
      p = control KR "pan" 0
      f = control KR "freq" 440
      d = control KR "decay" 1
      e = linen (impulse KR 0.1 0) 1e-2 1 d RemoveSynth
      s = pan2 (sinOsc AR f 0 * e * 0.3) p 1
  in  out o s

prepare :: IO ()
prepare = withSC3 reset

play_s01a :: IO ()
play_s01a = audition s01

-- play_s01b = withSC3 $ \fd -> do
--   async fd (d_recv $ synthdef "s01" s01)
--   send fd $ s_new "s01" (-1) AddToTail 1 [("freq",3300),("decay",0.3)]

s01' o p f d =
  let e = linen (impulse KR 0.1 0) 1e-2 1 d RemoveSynth
      s = pan2 (sinOsc AR f 0 * e * 0.3) p 1
  in  out o s

play_s01c = audition $ s01' 0 0 3300 0.3

-- play_s01d = audition $ ups "freq" 3300 $ ups "decay" 0.3 s01

-- ups :: String -> Double -> UGen -> UGen
-- ups key value ug = everywhere (mkT f) ug where
--   f (Control r key' _ t) | key == key' = Control r key value t
--   f x                    = x

-- play_s01e = audition . ups "freq" 3300 . ups "decay" 0.3 $ s01

-- upu :: String -> Double -> UGen -> UGen
-- upu key value ug = transform f ug where
--   f (Control r key' _ t) | key == key' = Control r key value t
--   f x                    = x

-- play_s01f x =
--   withSC3 . flip send . s_new "s01" (-1) AddToTail 1 $
--   [("freq",x),("decay",0.001)]

-- play_s01g x = audition $ upu "freq" x . upu "decay" 0.001 $ s01
