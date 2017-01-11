module Functions where

import Sound.SC3
import Sound.OSC
import Data.List
import Data.Maybe
import System.Environment
import Control.Monad.IO.Class

-- Sample Rate
samplerate :: Double
samplerate = 48000

-- Create group 1
g :: IO ()
g = withSC3 $ send $ g_new [(1, AddToTail, 0)]

-- Kill group 1 & 2
k :: IO ()
k = withSC3 $ do
  send $ n_free [1]
  ;send $ n_free [2]

-- Create group x
g' :: Int -> IO ()
g' x = withSC3 $ send $ g_new [(x, AddToTail, 0)]

-- Kill group x
k' :: Int -> IO ()
k' x = withSC3 $ send $ n_free [x]

-- Send control messages to node
c :: Int -> String -> Double -> IO ()
c node key value = withSC3 $ send $ n_set1 node key value

-- Send 2 control messages to node
c' :: Int -> String -> Double -> String -> Double -> IO ()
c' node k1 v1 k2 v2 =
  withSC3 $ do
  send (n_set1 node k1 v1)
  ;send (n_set1 node k2 v2)

-- Send list of control messages to node
c'' :: Int -> [(String, Double)] -> IO ()
c'' node msgList =
  sequence_
  $ map (\msg ->
           withSC3 $ send $ n_set1 node (fst msg) (snd msg)
        ) msgList

-- Create buffer & load file (fn)
rb :: Int -> String -> IO (Message)
rb bufferNumber fileName =
  withSC3 $ do
  async $ b_allocRead bufferNumber fileName 0 0

-- Concatenate home dir to file path (will eventually be used in sd)
hd :: [Char] -> [Char] -> IO [Char]
hd dirName fileName = do
  homeDir <- getEnv "HOME"
  ;return $ homeDir ++ dirName ++ fileName

-- Load samples in directory
sd :: [Char] -> [[Char]] -> [IO Message]
sd dirName fileList =
  map (\file ->
         withSC3 (do async (b_allocRead
                            (fromJust $ findIndex (file ==) fileList)
                            (dirName ++ file)
                            0 0)
                 )
      ) fileList

-- Query a buffer
bq :: Int -> IO ()
bq n = withSC3 $ do
  send $ b_query [n]
  ;r <- waitReply "/b_info"
  ;liftIO $ print r

-- Convert midi note number to hz
m2h :: Floating a => a -> a
m2h note = 440.0 * (2.0 ** ((note - 69.0) / 12.0))

-- Convert hz to midi note number
h2m :: (Floating a, Integral b, RealFrac a) => a -> b
h2m freq = round (69 + (12 * ((log (freq * 0.0022727272727)) / (log 2))))

-- Convert amplitude to decibels
a2d :: Floating a => a -> a
a2d amp = 20 * logBase 10 amp

-- Convert decibels to amplitude
d2a :: Floating r => r -> r
d2a db = exp $ log 10 * db / 20

-- Ternary Operator
(?) :: Bool -> (t, t) -> t
a ? (b, c) = if a then b else c

-- 0/1 == NoLoop/Loop
loopLogic :: (Eq a, Num a) => a -> Loop
loopLogic x = (x == 1) ? (Loop, NoLoop)

-- Amplify input
amp :: Num a => a -> a -> a
amp ampLevel input = input * ampLevel

-- Create & initialize synthdef
synthDef :: Int -> String -> UGen -> IO ()
synthDef node name input =
  withSC3 $ do async $ d_recv $ synthdef name $ out 0 input
               ;send $ s_new name node AddToTail 2 []
              -- ;send $ s_new name (-1) AddToTail 2 []

-- Master bus limiter
mbus :: Synthdef
mbus = synthdef "mbus" $ replaceOut 0 $ lmtr $ in' 2 AR 0

-- Initialize master bus limiter
i :: IO ()
i = withSC3 $ do
  send $ g_new [(1, AddToTail, 0)]
  ;send $ g_new [(2, AddToTail, 0)]
  ;async $ d_recv mbus
  ;send $ s_new "mbus" (-1) AddToTail 1 []

--
-- UGen Abstractions
--

lmtr :: UGen -> UGen
lmtr input = limiter input 0.99 0.001

cpos :: UGen
cpos = 0.1 * sinOsc KR 0.03 0

onepole :: UGen -> UGen -> UGen
onepole coef input = onePole input coef

lf :: UGen -> UGen -> UGen
lf frequency input = lpf input frequency

hf :: UGen -> UGen -> UGen
hf frequency input = hpf input frequency

fvrb :: UGen -> UGen
fvrb input = freeVerb input 0.5 1 1

fvrb' :: UGen -> UGen -> UGen -> UGen -> UGen
fvrb' mix room damp input = freeVerb input mix room damp

env :: UGen -> UGen -> UGen -> UGen -> UGen
env gate attack release input =
  input * envGen KR gate 1 0 1 RemoveSynth $ envASR attack 1 release EnvLin

tgrain :: UGen -> UGen -> UGen -> UGen -> UGen
tgrain bufNum rate centerPos duration =
  tGrains 2 (impulse AR 4 0) bufNum rate centerPos duration 0 1 1

tgrain' :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tgrain' bufNum rate centerPos duration ampLevel =
  tGrains 2 (impulse AR 4 0) bufNum rate centerPos duration 0 ampLevel 1

gvrb :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gvrb roomSize revTime damping inputbw spread dryLevel earlyRefLevel tailLevel maxRoomSize input =
  gVerb input roomSize revTime damping inputbw spread dryLevel earlyRefLevel tailLevel maxRoomSize

--
-- SynthDefs
--

tg :: Int -> Double -> Double -> Double -> Double -> IO ()
tg n b r a at =
  synthDef n "tg"
  $ amp ampL
  $ env gate att rel
  $ tgrain buf rate cpos dur
  where
    [buf, rate, ampL, dur, gate, att, rel] = control_set [control KR "b" b
                                                         ,control KR "r" r
                                                         ,control KR "a" a
                                                         ,control KR "d" 5
                                                         ,control KR "g" 1
                                                         ,control KR "at" at
                                                         ,control KR "rl" 40]

tgr :: Int -> Double -> Double -> Double -> Double -> IO ()
tgr n b r a at =
  synthDef n "tgr"
  $ amp ampL
  $ env gate att rel
  $ fvrb
  $ tgrain buf rate cpos dur
  where
    [buf, rate, ampL, dur, gate, att, rel] = control_set [control KR "b" b
                                                         ,control KR "r" r
                                                         ,control KR "a" a
                                                         ,control KR "d" 5
                                                         ,control KR "g" 1
                                                         ,control KR "at" at
                                                         ,control KR "rl" 40]

tgrf :: Int -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
tgrf n b r lff hff a at =
  synthDef n "tgrf"
  $ amp ampL
  $ env gate att rel
  $ fvrb
  $ hf highpass
  $ lf lowpass
  $ tgrain buf rate cpos dur
  where
    [buf, rate, lowpass, highpass, ampL, dur, gate, att, rel] = control_set [control KR "b" b
                                                                            ,control KR "r" r
                                                                            ,control KR "lff" lff
                                                                            ,control KR "hff" hff
                                                                            ,control KR "a" a
                                                                            ,control KR "d" 5
                                                                            ,control KR "g" 1
                                                                            ,control KR "at" at
                                                                            ,control KR "rl" 40]


tgg :: Int -> Double -> Double -> Double -> Double -> IO ()
tgg n b r a at =
  synthDef n "tgg"
  $ amp ampL
  $ env gate att rel
  $ gvrb 0.5 1.0 1.0 0.5 15 1 0.7 0.5 300
  $ tgrain' buf rate cpos dur 0.75
  where
    [buf, rate, ampL, dur, gate, att, rel] = control_set [control KR "b" b
                                                         ,control KR "r" r
                                                         ,control KR "a" a
                                                         ,control KR "d" 5
                                                         ,control KR "g" 1
                                                         ,control KR "at" at
                                                         ,control KR "rl" 40]

so :: Int -> Double -> Double -> Double -> IO ()
so n f a at =
  synthDef n "so"
  $ amp ampL
  $ env gate att rel
  $ sinOsc AR freq 1
  where
    [freq, ampL, gate, att, rel] = control_set [control KR "f" f
                                               ,control KR "a" a
                                               ,control KR "g" 1
                                               ,control KR "at" at
                                               ,control KR "rl" 40]

sor :: Int -> Double -> Double -> Double -> IO ()
sor n f a at =
  synthDef n "sor"
  $ amp ampL
  $ env gate att rel
  $ fvrb' 0.33 0.5 0.5
  $ sinOsc AR freq 1
  where
    [freq, ampL, gate, att, rel] = control_set [control KR "f" f
                                               ,control KR "a" a
                                               ,control KR "g" 1
                                               ,control KR "at" at
                                               ,control KR "rl" 40]

--
-- SynthDefs for c1.hs
--

c1tg :: Int -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
c1tg n b d r lff hff a rl =
  synthDef n "c1tg"
  $ amp ampL
  $ env gate 25 rel
  $ gvrb 15 6 0.5 0.5 20 0 0.7 0.5 300
  $ hf highpass
  $ lf lowpass
  $ tgrain' buf rate cpos dur 0.75
  where
    [buf, dur, rate, lowpass, highpass, ampL, rel, gate] = control_set [control KR "b" b
                                                                       ,control KR "d" d
                                                                       ,control KR "r" r
                                                                       ,control KR "lff" lff
                                                                       ,control KR "hff" hff
                                                                       ,control KR "a" a
                                                                       ,control KR "rl" rl
                                                                       ,control KR "g" 1]

c1tgl :: Int -> Double -> Double -> Double -> IO ()
c1tgl n b r a =
  synthDef n "c1tgl"
  $ amp ampL
  $ env gate 10 40
  $ hf 50
  $ gvrb 15 6 0.5 0.5 20 0.2 0.7 0.5 300
  $ lf 1000
  $ tGrains 1 (impulse AR 0.15 0) buf rate ((sinOsc KR 0.06 0) * 0.1) 7 0.5 1 1
  where
    [buf, rate, gate, ampL] = control_set [control KR "b" b
                                          ,control KR "r" r
                                          ,control KR "g" 1
                                          ,control KR "a" a]

c1sio :: Int -> Double -> Double -> Double -> IO ()
c1sio n f a rl =
  synthDef n "c1sio"
  $ amp ampL
  $ env gate 15 rel
  $ sinOsc AR (mce [freq, freq + 1]) 1
  where
    [freq, ampL, rel, gate] = control_set [control KR "f" f
                                          ,control KR "a" a
                                          ,control KR "rl" rl
                                          ,control KR "g" 1]

--
-- SynthDefs for c2.hs
--

c2s1pb :: Int -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
c2s1pb n b sp r lff hff a at rl =
  synthDef n "c2s1pb"
  $ amp ampL
  $ env gate att rel
  $ hf highpass
  $ lf lowpass
  $ playBuf 2 AR buf rate 1 spos Loop RemoveSynth
  where
    [buf, spos, rate, lowpass, highpass, ampL, att, rel, gate] = control_set [control KR "b" b
                                                                             ,control KR "sp" sp
                                                                             ,control KR "r" r
                                                                             ,control KR "lff" lff
                                                                             ,control KR "hff" hff
                                                                             ,control KR "a" a
                                                                             ,control KR "at" at
                                                                             ,control KR "rl" rl
                                                                             ,control KR "g" 1]


c2s2pb :: Int -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
c2s2pb n b sp r lff hff a at rl =
  synthDef n "c2s2pb"
  $ fvrb 
  $ env gate att rel
  $ hf highpass
  $ lf lowpass
  $ amp ampL
  $ playBuf 2 AR buf rate 1 spos (loopLogic loop) RemoveSynth
  where
    [buf, spos, rate, lowpass, highpass, ampL, att, rel, gate, loop] = control_set [control KR "b" b
                                                                                   ,control KR "sp" sp
                                                                                   ,control KR "r" r
                                                                                   ,control KR "lff" lff
                                                                                   ,control KR "hff" hff
                                                                                   ,control KR "a" a
                                                                                   ,control KR "at" at
                                                                                   ,control KR "rl" rl
                                                                                   ,control KR "g" 1
                                                                                   ,control KR "l" 1]


c2s2pbm :: Int -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
c2s2pbm n b sp r lff hff a at rl =
  synthDef n "c2s2pbm"
  $ fvrb 
  $ env gate att rel
  $ hf highpass
  $ lf lowpass
  $ amp ampL
  $ playBuf 1 AR buf rate 1 spos Loop RemoveSynth
  where
    [buf, spos, rate, lowpass, highpass, ampL, att, rel, gate] = control_set [control KR "b" b
                                                                             ,control KR "sp" sp
                                                                             ,control KR "r" r
                                                                             ,control KR "lff" lff
                                                                             ,control KR "hff" hff
                                                                             ,control KR "a" a
                                                                             ,control KR "at" at
                                                                             ,control KR "rl" rl
                                                                             ,control KR "g" 1]

c2s2pg :: Int -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
c2s2pg n b d cp r lff hff a at rl t =
  synthDef n "c2s2pg"
  $ fvrb 
  $ env gate att rel
  $ hf highpass
  $ lf lowpass
  $ tGrains 2 (impulse AR trig 0) buf rate cpos dur 0 ampL 1
  where
    [buf, dur, cpos, rate, lowpass, highpass, ampL, att, rel, trig, gate] = control_set [control KR "b" b
                                                                                        ,control KR "d" d
                                                                                        ,control KR "cp" cp
                                                                                        ,control KR "r" r
                                                                                        ,control KR "lff" lff
                                                                                        ,control KR "hff" hff
                                                                                        ,control KR "a" a
                                                                                        ,control KR "at" at
                                                                                        ,control KR "rl" rl
                                                                                        ,control KR "t" t
                                                                                        ,control KR "g" 1]

--
-- SynthDefs for c3.hs
--

c3s2wlf :: Int -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
c3s2wlf n b p a lff fs ws =
  synthDef n "c3s2wlf"
  $ amp ampL
  $ env gate 15 40
  $ lf lowpass
  $ warp1 1 buf ptr fscale wsize (-1) olaps 0.0 4
  where
    [buf, ptr, ampL, lowpass, fscale, wsize, olaps, gate] = control_set [control KR "b" b
                                                                        ,control KR "p" p
                                                                        ,control KR "a" a                  
                                                                        ,control KR "lff" lff
                                                                        ,control KR "fs" fs
                                                                        ,control KR "ws" ws
                                                                        ,control KR "ol" 1
                                                                        ,control KR "g" 1]

c3s2whf :: Int -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
c3s2whf n b p a hff fs ws =
  synthDef n "c3s2whf"
  $ amp ampL
  $ env gate 15 40
  $ hf highpass
  $ warp1 1 buf ptr fscale wsize (-1) olaps 0.0 4
  where
    [buf, ptr, ampL, highpass, fscale, wsize, olaps, gate] = control_set [control KR "b" b
                                                                         ,control KR "p" p
                                                                         ,control KR "a" a                  
                                                                         ,control KR "hff" hff
                                                                         ,control KR "fs" fs
                                                                         ,control KR "ws" ws
                                                                         ,control KR "ol" 1
                                                                         ,control KR "g" 1]

c3s2wr :: Int -> Double -> Double -> Double -> Double -> Double -> IO ()
c3s2wr n b p a fs ws =
  synthDef n "c3s2wr"
  $ amp ampL
  $ env gate 15 40
  $ fvrb' 1 1 1
  $ warp1 1 buf ptr fscale wsize (-1) olaps 0.0 4
  where
    [buf, ptr, ampL, fscale, wsize, olaps, gate] = control_set [control KR "b" b
                                                               ,control KR "p" p
                                                               ,control KR "a" a
                                                               ,control KR "fs" fs
                                                               ,control KR "ws" ws
                                                               ,control KR "ol" 1
                                                               ,control KR "g" 1]
