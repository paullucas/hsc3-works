import Sound.SC3
:load functions.hs

g
o (sw 100 0.1)
o (s 220 0.1)
mono (sw 100 0.1)
mono (s 220 0.1)
k

g
rb 0 "/home/paulll/Producing/duplicates/48/wav/1-4.wav"
o (lp 0 1 0.5)
o (lpm 0 1 0.5)
k
