:m + Sound.SC3 Sound.OSC
:l functions.hs

sequence_
  $ sd "/Users/paul/Producing/april20th-2017/samples/"
  $ map (\x -> (show x) ++ "f.wav") [1..4]

sequence_
  $ sd "/home/paulll/Producing/april20th-2017/samples/"
  $ map (\x -> (show x) ++ "f.wav") [1..4]

g
c6w 2 0 0.1 2 0.08 10 2000 50 20 40
c 2 "ol" 2
c6w 3 1 0.1 0.8 0.2 9 2000 80 15 40
c6w 4 2 0.5 1 0.4 8 4000 200 25 40
c 2 "ol" 4
c 3 "ol" 3
c 4 "ol" 5
c 2 "fs" 0.06
c 3 "fs" 0.14
c 4 "fs" 0.17
c5wr 5 3 0.0 2 0.2 15 4000 100
c4sio 6 90 0.4 40
c 3 "fs" 0.18
c 2 "fs" 0.09
c 4 "fs" 0.19
c4sio 7 3000 0.004 30
c 7 "g" 0
c 2 "g" 0
c 3 "fs" 0.16
c 5 "g" 0
c 3 "g" 0
c 4 "ol" 1
c 4 "g" 0
c 6 "g" 0
k
