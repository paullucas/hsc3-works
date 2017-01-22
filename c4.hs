:m + Sound.SC3 Sound.OSC
:l functions.hs

sequence_
  $ sd "/home/paulll/Producing/jan2nd-2017/smp/"
  $ map (\x -> "s" ++ (show x) ++ "m.wav") [0..8]

i
c4tgr 3 1 0.67 0.8 15
c4tgrf 4 3 800 2000 400 0.8 10
c 4 "r" 0.8
c 3 "r" 0.625
c 4 "r" 0.75
c 3 "r" 0.55
c 4 "r" 0.4
c4sio 5 5250 0.012 10
c 5 "g" 0
c4sio 6 800 0.15 10
c' 6 "f" 80 "a" 0.8
c4sio 7 164.81 0.3 10
c4sio 8 207.65 0.3 10
c4sio 9 880 0.04 10
c 9 "g" 0
c4sio 10 7040 0.015 10
c 10 "g" 0
c' 6 "f" 123.47 "a" 0.8
c' 6 "f" 40 "a" 1.6
do c 8 "g" 0; c 7 "g" 0
c 6 "g" 0
do c 4 "g" 0; c 3 "g" 0
k
