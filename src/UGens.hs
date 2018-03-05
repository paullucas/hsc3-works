module UGens where

import           Sound.SC3.Common.Envelope
import           Sound.SC3.UGen
import           Sound.SC3.UGen.Bindings   hiding (gate, mix)

k :: String -> Double -> UGen
k = control KR

env :: UGen -> UGen -> UGen -> UGen -> UGen
env gate attack release input =
  input * envGen KR gate 1 0 1 RemoveSynth (envASR attack 1 release EnvLin)

o :: UGen -> UGen -> UGen
o db input = out 0 $ input * db

lpf :: UGen -> UGen -> UGen
lpf freq in_ =
  mkUGen
    Nothing
    [KR, AR]
    (Right [0])
    "LPF"
    [in_, freq]
    Nothing
    1
    (Special 0)
    NoId

hpf :: UGen -> UGen -> UGen
hpf freq in_ =
  mkUGen
    Nothing
    [KR, AR]
    (Right [0])
    "HPF"
    [in_, freq]
    Nothing
    1
    (Special 0)
    NoId

limiter :: UGen -> UGen -> UGen -> UGen
limiter level dur in_ =
  mkUGen
    Nothing
    [AR]
    (Right [0])
    "Limiter"
    [in_, level, dur]
    Nothing
    1
    (Special 0)
    NoId

freeVerb :: UGen -> UGen -> UGen -> UGen -> UGen
freeVerb mix room damp in_ =
  mkUGen
    Nothing
    [AR]
    (Right [0])
    "FreeVerb"
    [in_, mix, room, damp]
    Nothing
    1
    (Special 0)
    NoId

gVerb ::
     UGen
  -> UGen
  -> UGen
  -> UGen
  -> UGen
  -> UGen
  -> UGen
  -> UGen
  -> UGen
  -> UGen
  -> UGen
gVerb roomsize revtime damping inputbw spread drylevel earlyreflevel taillevel maxroomsize in_ =
  mkUGen
    Nothing
    [AR]
    (Right [0])
    "GVerb"
    [ in_
    , roomsize
    , revtime
    , damping
    , inputbw
    , spread
    , drylevel
    , earlyreflevel
    , taillevel
    , maxroomsize
    ]
    Nothing
    2
    (Special 0)
    NoId

pbuf :: UGen -> UGen -> UGen -> UGen -> UGen
pbuf buffer rate trigger startPos =
  playBuf 2 AR buffer rate trigger startPos Loop RemoveSynth
