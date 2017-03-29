:m + Sound.SC3 Sound.OSC
:l functions.hs

sequence_
  $ sd "/Users/paul/Producing/november4th-2016/chops/smpls-mono/"
  ["s7m.wav", "s8m.wav"]

sequence_
  $ sd "/home/paulll/Producing/november4th-2016/chops/smpls-mono/"
  ["s7m.wav", "s8m.wav"]

g
c1tg 2 1 5 0.6 2000 125 0.5 40
c1tgl 3 0 0.2
c 2 "r" 0.4
c 2 "r" 0.35
c 3 "r" 0.4
c 3 "r" 0.36
c 2 "r" 0.55
c 3 "r" 0.5
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
c'' 3 [("r",0.18),("g",0)]
k
