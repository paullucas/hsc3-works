import Sound.SC3
:load functions.hs

let f = map ("/home/paulll/Producing/duplicates/48/wav/" ++) ["1-4.wav", "1-7.wav"]

g
o (sw 100 0.1)
o (s 220 0.1)
mono (sw 100 0.1)
mono (s 220 0.1)
k

g
rb 0 (f !! 0)
rb 1 (f !! 1)
o (lp 0 1 0.5)
o (lp 1 1 0.5)
k
