import Sound.SC3

g = withSC3 (send (g_new [(1, AddToTail, 0)]))
k = withSC3 (send (n_free [1]))
mono x = audition $ mce [out 0 x, out 1 x]
s f a = (sinOsc AR f 0 * a)
sw f a = (saw AR f) * a
