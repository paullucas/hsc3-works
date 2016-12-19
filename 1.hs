import Sound.SC3
:load functions.hs

withSC3 (send (g_new [(1, AddToTail, 0)]))
audition (out 0 (sinOsc AR 440 0 * 0.1))
withSC3 (send (n_free [1]))

g
mono (sw 100 0.1)
mono (s 220 0.1)
k
