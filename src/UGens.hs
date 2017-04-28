module UGens where

import Sound.SC3

k :: String -> Double -> UGen
k key value = control KR key value

lmtr :: UGen -> UGen
lmtr input = limiter input 0.8 0.001

cpos :: UGen
cpos = 0.1 * sinOsc KR 0.03 0

onepole :: UGen -> UGen -> UGen
onepole coef input = onePole input coef

lf :: UGen -> UGen -> UGen
lf frequency input = lpf input frequency

hf :: UGen -> UGen -> UGen
hf frequency input = hpf input frequency

fvrb :: UGen -> UGen
fvrb input = freeVerb input 0.5 1 1

fvrb' :: UGen -> UGen -> UGen -> UGen -> UGen
fvrb' mix room damp input = freeVerb input mix room damp

env :: UGen -> UGen -> UGen -> UGen -> UGen
env gate attack release input =
  input * envGen KR gate 1 0 1 RemoveSynth (envASR attack 1 release EnvLin)

tgrain :: UGen -> UGen -> UGen -> UGen -> UGen
tgrain bufNum rate centerPos duration =
  tGrains 2 (impulse AR 4 0) bufNum rate centerPos duration 0 1 1

tgrain' :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tgrain' bufNum rate centerPos duration ampLevel =
  tGrains 2 (impulse AR 4 0) bufNum rate centerPos duration 0 ampLevel 1

gvrb :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gvrb roomSize revTime damping inputbw spread dryLevel earlyRefLevel tailLevel maxRoomSize input =
  gVerb input roomSize revTime damping inputbw spread dryLevel earlyRefLevel tailLevel maxRoomSize

pan :: UGen -> UGen
pan input = pan2 input 0 1

mX :: UGen
mX = mouseX KR 0 1 Linear 0.2

mY :: UGen
mY = mouseY KR 0 1 Linear 0.2
