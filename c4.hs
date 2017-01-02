:m + Sound.SC3 Sound.OSC
:l functions.hs

sequence_
  $ sd "/home/paulll/Producing/jan2nd-2017/smp/"
  $ map (\x -> "s" ++ (show x) ++ "m.wav") [0..8]

g
tgr 3 1 0.67 1 15
tgrf 4 3 800 2000 400 1 10
c 4 "r" 0.8
c 3 "r" 0.625
c 4 "r" 0.75
c 3 "r" 0.55
c 4 "r" 0.4
c1sio 5 5250 0.02 10
c 5 "g" 0
c1sio 6 800 0.15 10
c' 6 "f" 80 "a" 0.8
k
