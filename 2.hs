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

-- simplify control messages
let ctl x y = withSC3 (send (n_set1 100 x y))
ctl "freq" 600

-- create more dynamic group/kill functions (take # as arg)
let g x = withSC3 (send (g_new [(x, AddToTail, 0)]))
let k x = withSC3 (send (n_free [x]))

-- create a synthdef, play on group 2, kill on group 2
g 2
let {[f, a] = control_set [control KR "freq" 440
                          ,control KR "amp" 0.2]}
in withSC3 (do async (d_recv (synthdef "g" (out 0 (sinOsc AR f 0 * a))))
           ;send (s_new "g" 100 AddToTail 2 []))
k 2
