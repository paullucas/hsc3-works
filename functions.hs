module Functions where

import Sound.SC3
import Sound.OSC
import Data.List
import Data.Maybe

-- functions

g = withSC3 (send (g_new [(1, AddToTail, 0)]))
k = withSC3 (send (n_free [1]))

gx x = withSC3 (send (g_new [(x, AddToTail, 0)]))
kx x = withSC3 (send (n_free [x]))

ctl x y z = withSC3 (send (n_set1 x y z))

o x = audition x
mono x = audition $ mce [out 0 x, out 1 x]

rb bn fn =
  withSC3 (do
    async (b_allocRead bn fn 0 0))

-- Synthdefs

so f a =
  let {[freq, amp] = control_set [control KR "f" f, control KR "a" a];
        s = (sinOsc AR freq 0 * amp)}
  in withSC3 (do async (d_recv (synthdef "g" (out 0 s)))
                 ;send (s_new "g" 100 AddToTail 1 []))

---

-- sd dn fl =
--   map (\x -> withSC3 (do async (b_allocRead (fromJust (findIndex (x ==) fl)) (dn ++ x) 0 0))) fl

---

-- so f a = (sinOsc AR f 0 * a)
-- sw f a = (saw AR f) * a

-- lp b r ph  =
--   bufRdC 2 AR b ((lfSaw AR (r * 0.5/bufDur KR b) 0 + ph) * bufFrames KR b) Loop

-- lpm b r ph =
--   bufRdC 1 AR b ((lfSaw AR (r * 0.5/(bufDur KR b)) 0 + ph) * bufFrames KR b) Loop

---

-- How can I query nodes / groups?

-- withSC3 (send (n_query [(100)]))
-- withSC3 (send (n_query [(101)]))

-- getTheNode n = do
--   sendOSC $ g_queryTree [(n,True)]
--   m <- waitReply "/g_queryTree.reply"
--   return $ m

-- getTheNode n = do
--   sendOSC $ g_queryTree [(n,True)]
--   m <- waitReply "/g_queryTree.reply"
--   return $ parseNode m

-- withSC3 (do send (g_queryTree [(1,True)]))
