import Sound.SC3
import Sound.OSC
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
