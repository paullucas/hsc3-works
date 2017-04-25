:m + Sound.SC3 Sound.OSC
:l functions.hs

sequence_
  $ sd "/Users/paul/Producing/december30th-2016/"
  $ map (\x -> "s" ++ (show x) ++ ".wav") [0..4]

sequence_
  $ sd "/home/paulll/Producing/december30th-2016/"
  $ map (\x -> "s" ++ (show x) ++ ".wav") [0..4]

g
c1tg 2 1 5 0.6 2000 125 0.17 40
c1tgl 3 0 0.2 0.8
c 2 "r" 0.4
c 2 "r" 0.35
c 3 "r" 0.4
c 3 "r" 0.36
c 2 "r" 0.55
c' 3 "r" 0.5 "a" 0.5
(do c 2 "r" 0.2; c 3 "r" 0.2)
c1sio 4 66.5 0.5 40
c 4 "f" 58.4
c1tg 5 1 10 2 8000 400 0.45 20
c 5 "r" 1.4
c 4 "g" 0
c1sio 6 (m2h 114) 0.020 10
c 6 "g" 0
c 5 "g" 0
(do c 2 "r" 0.36; c 3 "r" 0.4)
c 3 "r" 0.36
c 2 "g" 0
c 3 "r" 0.2
c' 3 "r" 0.18 "g" 0
f

g
c3s2wlf 10 2 0.83 1.5 4000 0.4 10
c3s2wr 11 3 0.27 1.2 0.5 9
c 10 "fs" 0.25
c 10 "ws" 12
c' 11 "fs" 0.5 "ws" 9
c' 11 "fs" 0.675 "ws" 13
c 11 "ol" 2
c' 10 "fs" 0.4 "ws" 14
c 10 "ol" 3
c' 11 "fs" 0.275 "ws" 10
c 10 "g" 0
c 11 "ol" 1
c 11 "fs" 0.22
c3s2whf 12 4 0.23 1.4 400 0.8 16
c 12 "p" 0.7
c 12 "fs" 1.1
c'' 12 [("fs",0.65),("p",0.1),("ol",2)]
c 11 "g" 0
c 12 "g" 0
f
