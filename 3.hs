:m + Sound.SC3 Sound.OSC
:l functions.hs

g
so 2 (m2h 63) 0.2 2
c 2 "f" (m2h 65)
c 2 "f" (m2h 68)
c 2 "f" (m2h 75)
c 2 "f" 349.2282314330039
c 2 "f" 415.3046975799451
c 2 "f" 622.2539674441618
c 2 "f" 311.1269837220809
k

rb 0 "/home/paulll/Producing/April5th-2016/samples/5.wav"

g
tg 3 0 0.8 0.5 25
c 3 "r" 0.5
c 3 "r" 0.35
c 3 "g" 0
k

g
so 4 440 0.2 25
c 4 "f" (m2h 64)
c 4 "g" 0
k
