:m + Sound.SC3 Sound.OSC
:l functions.hs

sequence_
  $ sd "/home/paulll/Producing/october20th-2016/sc8/"
  $ map (\x -> "s" ++ (show x) ++ ".wav") [0..17]

g
c2s1pb 2 1 (samplerate / 4) 0.4 1500 150 2 10 1
c2s1pb 3 1 0.0 0.5 3000 50 1 1 5
c2s1pb 4 1 (samplerate / 8) 0.8 2000 40 1 1 5
c2s1pb 5 0 0.0 0.2 3000 50 2 10 10
c2s1pb 6 0 (samplerate / 2) 0.4 5000 40 1 10 10
c2s1pb 7 1 0.0 1 800 350 1 1 10
c 2 "r" 0.6
c 3 "r" 0.3
c 4 "r" 0.6
c 5 "r" 0.3
c 5 "r" 0.225
c 5 "r" 0.2
c 5 "r" 0.3
c 5 "r" 0.225
c 5 "r" 0.2
c 6 "r" 0.4
c 7 "r" 0.5
c 7 "r" 0.9
c 2 "g" 0
c 3 "g" 0
c 4 "g" 0
c 6 "g" 0
c 7 "g" 0
c 5 "g" 0
k

g
c2s2pb 10 2 0 0.4 4000 100 0.8 10 10
c2s2pbm 11 3 0 0.5 2000 50 0.93 10 10
c 10 "g" 0
c2s2pb 12 2 0 0.3 3000 100 0.8 10 10
c 11 "g" 0
c2s2pbm 13 3 0 0.3 2000 30 0.93 10 10
c 12 "g" 0
c2s2pb 14 2 0 0.27 4000 80 0.8 10 10
c 14 "g" 0
c2s2pb 15 2 0 0.2 3500 100 0.8 10 10
c2s2pg 16 4 25 1222 0.8 2000 1000 0.9 10 20 0.165
c 15 "g" 0
c 13 "g" 0
c 16 "g" 0
k
