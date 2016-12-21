import Sound.SC3
import Sound.OSC
import Data.List
import Data.Maybe

g = withSC3 (send (g_new [(1, AddToTail, 0)]))
k = withSC3 (send (n_free [1]))

o x = audition x
mono x = audition $ mce [out 0 x, out 1 x]

so f a = (sinOsc AR f 0 * a)
sw f a = (saw AR f) * a

rb bn fn =
  withSC3 (do
    async (b_allocRead bn fn 0 0))

-- sd dn fl =
--   map (\x -> withSC3 (do async (b_allocRead (fromJust (findIndex (x ==) fl)) (dn ++ x) 0 0))) fl

lp b r ph  =
  bufRdC 2 AR b ((lfSaw AR (r * 0.5/bufDur KR b) 0 + ph) * bufFrames KR b) Loop

lpm b r ph =
  bufRdC 1 AR b ((lfSaw AR (r * 0.5/(bufDur KR b)) 0 + ph) * bufFrames KR b) Loop
