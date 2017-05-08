module UGens where

import Sound.SC3.UGen
import Sound.SC3.UGen.Bindings

k :: String -> Double -> UGen
k key value = control KR key value

env :: UGen -> UGen -> UGen -> UGen -> UGen
env gate attack release input =
  input * envGen KR gate 1 0 1 RemoveSynth (envASR attack 1 release EnvLin)

tgrain :: UGen -> UGen -> UGen -> UGen -> UGen
tgrain bufNum rate centerPos duration =
  tGrains 2 (impulse AR 4 0) bufNum rate centerPos duration 0 1 1

tgrain' :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tgrain' bufNum rate centerPos duration ampLevel =
  tGrains 2 (impulse AR 4 0) bufNum rate centerPos duration 0 ampLevel 1

pan :: UGen -> UGen
pan input = pan2 input 0 1

amp :: UGen -> UGen -> UGen
amp ampLevel input = out 0 $ input * ampLevel

lpf :: UGen -> UGen -> UGen
lpf freq in_ = mkUGen Nothing [KR,AR] (Right [0]) "LPF" [in_,freq] Nothing 1 (Special 0) NoId

hpf :: UGen -> UGen -> UGen
hpf freq in_ = mkUGen Nothing [KR,AR] (Right [0]) "HPF" [in_,freq] Nothing 1 (Special 0) NoId

limiter :: UGen -> UGen -> UGen -> UGen
limiter level dur in_ = mkUGen Nothing [AR] (Right [0]) "Limiter" [in_,level,dur] Nothing 1 (Special 0) NoId

freeVerb :: UGen -> UGen -> UGen -> UGen -> UGen
freeVerb mix room damp in_ = mkUGen Nothing [AR] (Right [0]) "FreeVerb" [in_,mix,room,damp] Nothing 1 (Special 0) NoId

gVerb :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gVerb roomsize revtime damping inputbw spread drylevel earlyreflevel taillevel maxroomsize in_ = mkUGen Nothing [AR] (Right [0]) "GVerb" [in_,roomsize,revtime,damping,inputbw,spread,drylevel,earlyreflevel,taillevel,maxroomsize] Nothing 2 (Special 0) NoId
