import Sound.SC3
import Sound.OSC
:l functions.hs

-- synthdef + control msg example from docs
g
let {[f,a] = control_set [control KR "freq" 440
                         ,control KR "amp" 0.1]
    ;g = synthdef "g" (out 0 (sinOsc AR f 0 * a))
    ;c = do {_ <- async (d_recv g)
            ;send (s_new "g" 100 AddToTail 1 [])}}
  in withSC3 c
withSC3 (send (n_set1 100 "freq" 430))
withSC3 (send (n_set1 100 "amp" 0.1))
k

-- rewrite synthdef + control msg example from docs
g
let {[f, a] = control_set [control KR "freq" 440
                          ,control KR "amp" 0.2]}
in withSC3 (do async (d_recv (synthdef "g" (out 0 (sinOsc AR f 0 * a))))
           ;send (s_new "g" 100 AddToTail 1 []))
withSC3 (send (n_set1 100 "freq" 430))
withSC3 (send (n_set1 100 "amp" 0.1))
k


-- create synthdef, play on group 2, kill on group 2
gx 2
let {[f, a] = control_set [control KR "freq" 440
                          ,control KR "amp" 0.2]}
in withSC3 (do async (d_recv (synthdef "g" (out 0 (sinOsc AR f 0 * a))))
           ;send (s_new "g" 100 AddToTail 2 []))
kx 2


-- Run 2 synths
g

let {[f, a] = control_set [control KR "freq" 440
                          ,control KR "amp" 0.2]}
in withSC3 (do async (d_recv (synthdef "g" (out 0 (sinOsc AR f 0 * a))))
           ;send (s_new "g" 101 AddToTail 1 []))

let {[f, a] = control_set [control KR "freq" 440
                          ,control KR "amp" 0.2]}
in withSC3 (do async (d_recv (synthdef "g" (out 0 (sinOsc AR f 0 * a))))
           ;send (s_new "g" 100 AddToTail 1 []))

ctl 100 "freq" 250
ctl 101 "freq" 280

k
