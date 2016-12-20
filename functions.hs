import Sound.SC3

g = withSC3 (send (g_new [(1, AddToTail, 0)]))
k = withSC3 (send (n_free [1]))

o x = audition x
mono x = audition $ mce [out 0 x, out 1 x]

s f a = (sinOsc AR f 0 * a)
sw f a = (saw AR f) * a

rb bn fn =
  withSC3 (do
    async (b_allocRead bn fn 0 0))

lp b r ph  =
  bufRdC 2 AR b ((lfSaw AR (r * 0.5/bufDur KR b) 0 + ph) * bufFrames KR b) Loop

lpm b r ph =
  bufRdC 1 AR b ((lfSaw AR (r * 0.5/(bufDur KR b)) 0 + ph) * bufFrames KR b) Loop

-- lp b r ph  =
--   let dur = bufDur KR b
--       fr = bufFrames KR b
--       pos = (lfSaw AR (r * 0.5/dur) 0 + ph) * fr
--   in
--   bufRdC 2 AR b pos Loop

-- lpm b r ph  =
--   let dur = bufDur KR b
--       fr = bufFrames KR b
--       pos = (lfSaw AR (r * 0.5/dur) 0 + ph) * fr
--   in
--   bufRdC 1 AR b pos Loop
