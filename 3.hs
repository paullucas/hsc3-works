:m + Sound.SC3 Sound.OSC
:l functions.hs

g
so 320 0.2

ctl 100 "f" 420
ctl 100 "a" 0.3

k


g

let {[f, a] = control_set [control KR "f" 440, control KR "a" 0.2];
     s = (sinOsc AR f 0 * a)}
  in withSC3 (do async (d_recv (synthdef "g" (out 0 s)))
                 ;send (s_new "g" 100 AddToTail 1 []))
