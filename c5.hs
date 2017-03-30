:m + Sound.SC3 Sound.OSC
:l functions.hs

sequence_
  $ sd "/Users/paul/Producing/feb28-2017/f/"
  $ map (\x -> (show x) ++ "mf.wav") [1..4]

sequence_
  $ sd "/home/paulll/Producing/feb28-2017/f/"
  $ map (\x -> (show x) ++ "mf.wav") [1..4]

g
c5wr 2 0 0.4 4 0.6 0.8 2000 60
c' 2 "p" 0.7 "ws" 0.2
c' 2 "fs" 0.8 "ws" 0.8
c' 2 "fs" 0.6 "p" 0.65
c' 2 "p" 0.9 "ws" 2
c 2 "fs" 0.66
c5wr 3 2 0.3 4 0.6 5 4000 400
c' 3 "p" 0.2 "ws" 5.5
c' 3 "fs" 3.5 "ws" 6
c 2 "fs" 1.1
c 2 "fs" 0.9
c 2 "fs" 0.6
c 3 "fs" 0.6
c' 2 "fs" 0.8 "ws" 0.2
c' 2 "fs" 1.8 "ws" 0.14
c' 2 "fs" 0.8 "ws" 0.215
c' 2 "fs" 0.4 "ws" 0.2
c 2 "g" 0
c' 3 "ws" 8 "g" 0
k

g
c5pb 4 1 0 0.8 2000 50 2 2 5
c 4 "r" 0.8
c 4 "r" 0.6
c 4 "r" 0.55
k

g
c5wm 2 0 4 0.6 2000 60
c 2 "fs" 0.825
c 2 "fs" 0.6
c 2 "fs" 0.65
c5wm 3 0 4 8 2000 60
c 3 "fs" 44
c 3 "fs" 47
c 3 "fs" 50
c 3 "fs" 57
c 3 "fs" 52.5
c 3 "fs" 54
--
c 2 "fs" 0.725
c 2 "fs" 0.675
c 2 "fs" 0.62
c 3 "fs" 66
--
c 2 "fs" 0.925
c 2 "fs" 0.85
c 2 "fs" 0.62
c 2 "fs" 1.4
c 3 "fs" 38
--
c 2 "g" 0
c 3 "g" 0
k


