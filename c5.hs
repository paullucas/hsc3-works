:m + Sound.SC3 Sound.OSC
:l functions.hs

sequence_
  $ sd "/Users/paul/Producing/feb28-2017/f/"
  $ map (\x -> (show x) ++ "mf.wav") [1..2]

i
c2s1pb 4 1 0 1.2 4000 60 1.5 20 20
c2s1pb 5 0 0 0.4 1500 150 2 20 20
c 5 "r" 0.8
c 4 "r" 0.7
c 5 "r" 0.6
c 5 "r" 0.8
c 5 "r" 0.6
c 5 "r" 0.66
c 4 "r" 0.78
c 5 "r" 0.62
c 4 "g" 0
c 5 "g" 0
k
