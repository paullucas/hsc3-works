import Sound.SC3

withSC3 (send (g_new [(1, AddToTail, 0)]))
audition (out 0 (sinOsc AR 440 0 * 0.1))
withSC3 (send (n_free [1]))

let g = withSC3 (send (g_new [(1, AddToTail, 0)]))
let s frq = audition (out 0 (sinOsc AR frq 0 * 0.1))
