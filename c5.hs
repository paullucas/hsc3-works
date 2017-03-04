:m + Sound.SC3 Sound.OSC
:l functions.hs

sequence_
  $ sd "/Users/paul/Producing/feb28-2017/f/"
  $ map (\x -> (show x) ++ "mf.wav") [1..4]

i
c5wr 7 0 0.4 4 0.6 0.8 2000 60
c' 7 "p" 0.7 "ws" 0.2
c' 7 "fs" 0.8 "ws" 0.8
c' 7 "fs" 0.6 "p" 0.65
c' 7 "p" 0.9 "ws" 2
c 7 "fs" 0.66
c5wr 8 2 0.3 4 0.6 5 4000 400
c' 8 "p" 0.2 "ws" 5.5
c' 8 "fs" 3.5 "ws" 6
c 7 "fs" 1.1
c 7 "fs" 0.9
c 7 "fs" 0.6
c 8 "fs" 0.6
c' 7 "fs" 0.8 "ws" 0.2
c' 7 "fs" 1.8 "ws" 0.14
c' 7 "fs" 0.8 "ws" 0.215
c' 7 "fs" 0.4 "ws" 0.2
c 7 "g" 0
c' 8 "ws" 8 "g" 0
k
