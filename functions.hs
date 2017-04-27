module Functions where

import Sound.SC3
import Sound.OSC
import Data.List
import Data.Maybe
import System.Environment
import Control.Monad.IO.Class
import System.IO.Unsafe
import Data.IORef

-- Sample Rate
samplerate :: Double
samplerate = 48000

-- Create group 1
g :: IO ()
g = withSC3 $ send $ g_new [(1, AddToTail, 0)]

-- Free group 1
f :: IO ()
f = withSC3 $ send $ n_free [1]

-- Create group x
g' :: Int -> IO ()
g' x = withSC3 $ send $ g_new [(x, AddToTail, 0)]

-- Free group x
f' :: Int -> IO ()
f' x = withSC3 $ send $ n_free [x]

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

-- Query a node
nq :: Int -> IO ()
nq n = withSC3 $ do
  send $ n_query [n]
  ;r <- waitReply "/n_info"
  ;liftIO $ print r

nodeCounter :: IORef Integer
nodeCounter = unsafePerformIO $ newIORef 2

nn :: IO Integer
nn = do
  i <- readIORef nodeCounter
  writeIORef nodeCounter $ i+1
  return i

n = do
  i <- readIORef nodeCounter
  return i

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
synthDef :: String -> UGen -> IO Int
synthDef name input = do
  node <- nn
  ;withSC3
    (do async $ d_recv $ synthdef name $ out 0 input
        ;send $ s_new name (fromIntegral node) AddToTail 1 [])
  ;return (fromIntegral node)

-- -- Master bus limiter
-- mbus :: Synthdef
-- mbus = synthdef "mbus" $ replaceOut 0 $ lmtr $ hf 40 $ in' 2 AR 0

-- -- Initialize master bus limiter
-- i :: IO ()
-- i = withSC3 $ do
--   send $ g_new [(1, AddToTail, 0)]
--   ;send $ g_new [(2, AddToTail, 0)]
--   ;async $ d_recv mbus
--   ;send $ s_new "mbus" (-1) AddToTail 1 []

-- recInit
-- g
-- recStart
-- ...
-- recStop
-- f

d_SC3_Recorder :: SC3_Recorder
d_SC3_Recorder =
  SC3_Recorder {rec_sftype = Wave
               ,rec_coding = PcmFloat
               ,rec_fname = "/tmp/sc3-recorder.wav"
               ,rec_nc = 2
               ,rec_bus = 0
               ,rec_buf_id = 10
               ,rec_buf_frames = 65536
               ,rec_node_id = 2001
               ,rec_group_id = 0
               ,rec_dur = Just 60}

recInit = withSC3
  $ sendBundle
  $ bundle immediately
  $ rec_init_m d_SC3_Recorder

recStart = withSC3
  $ sendMessage
  $ rec_begin_m d_SC3_Recorder

recStop = withSC3
  $ sendBundle
  $ bundle immediately
  $ rec_end_m d_SC3_Recorder

--
-- UGen Abstractions
--

k :: String -> Double -> UGen
k key value = control KR key value

lmtr :: UGen -> UGen
lmtr input = limiter input 0.8 0.001
-- lmtr input = limiter input 0.95 0.001
-- lmtr input = limiter input 0.99 0.001

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
  input * envGen KR gate 1 0 1 RemoveSynth (envASR attack 1 release EnvLin)

tgrain :: UGen -> UGen -> UGen -> UGen -> UGen
tgrain bufNum rate centerPos duration =
  tGrains 2 (impulse AR 4 0) bufNum rate centerPos duration 0 1 1

tgrain' :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tgrain' bufNum rate centerPos duration ampLevel =
  tGrains 2 (impulse AR 4 0) bufNum rate centerPos duration 0 ampLevel 1

gvrb :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gvrb roomSize revTime damping inputbw spread dryLevel earlyRefLevel tailLevel maxRoomSize input =
  gVerb input roomSize revTime damping inputbw spread dryLevel earlyRefLevel tailLevel maxRoomSize

pan :: UGen -> UGen
pan input = pan2 input 0 1

mX = mouseX KR 0 1 Linear 0.2

mY = mouseY KR 0 1 Linear 0.2

--
-- SynthDefs
--

tg :: Double -> Double -> Double -> Double -> IO Int
tg b r a at =
  synthDef "tg"
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

tgr :: Double -> Double -> Double -> Double -> IO Int
tgr b r a at =
  synthDef "tgr"
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

tgrf :: Double -> Double -> Double -> Double -> Double -> Double -> IO Int
tgrf b r lff hff a at =
  synthDef "tgrf"
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


tgg :: Double -> Double -> Double -> Double -> IO Int
tgg b r a at =
  synthDef "tgg"
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

so :: Double -> Double -> Double -> IO Int
so f a at =
  synthDef "so"
  $ amp ampL
  $ env gate att rel
  $ sinOsc AR freq 1
  where
    [freq, ampL, gate, att, rel] = control_set [control KR "f" f
                                               ,control KR "a" a
                                               ,control KR "g" 1
                                               ,control KR "at" at
                                               ,control KR "rl" 40]

sor :: Double -> Double -> Double -> IO Int
sor f a at =
  synthDef "sor"
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

c1tg :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO Int
c1tg b d r lff hff a rl =
  synthDef "c1tg"
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

c1tgl :: Double -> Double -> Double -> IO Int
c1tgl b r a =
  synthDef "c1tgl"
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

c1sio :: Double -> Double -> Double -> IO Int
c1sio f a rl =
  synthDef "c1sio"
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

c2s1pb :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO Int
c2s1pb b sp r lff hff a at rl =
  synthDef "c2s1pb"
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


c2s2pb :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO Int
c2s2pb b sp r lff hff a at rl =
  synthDef "c2s2pb"
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


c2s2pbm :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO Int
c2s2pbm b sp r lff hff a at rl =
  synthDef "c2s2pbm"
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

c2s2pg :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO Int
c2s2pg b d cp r lff hff a at rl t =
  synthDef "c2s2pg"
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

c3s2wlf :: Double -> Double -> Double -> Double -> Double -> Double -> IO Int
c3s2wlf b p a lff fs ws =
  synthDef "c3s2wlf"
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

c3s2whf :: Double -> Double -> Double -> Double -> Double -> Double -> IO Int
c3s2whf b p a hff fs ws =
  synthDef "c3s2whf"
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

c3s2wr :: Double -> Double -> Double -> Double -> Double -> IO Int
c3s2wr b p a fs ws =
  synthDef "c3s2wr"
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

--
-- SynthDefs for c4.hs
--

c4tgr :: Double -> Double -> Double -> Double -> IO Int
c4tgr b r a at =
  synthDef "c4tgr"
  $ amp ampLevel
  $ env gate attack release
  $ lmtr
  $ fvrb
  $ hf 40
  $ tgrain buffer rate cpos duration
  where
    buffer   = k "b" b
    rate     = k "r" r
    ampLevel = k "a" a
    duration = k "d" 5
    gate     = k "g" 1
    attack   = k "at" at
    release  = k "rl" 40

c4tgrf :: Double -> Double -> Double -> Double -> Double -> Double -> IO Int
c4tgrf b r lff hff a at =
  synthDef "c4tgrf"
  $ amp ampLevel
  $ env gate attack release
  $ lmtr
  $ fvrb
  $ hf highpassFreq
  $ lf lowpassFreq
  $ tgrain buffer rate cpos duration
  where
    buffer       = k "b" b
    rate         = k "r" r
    lowpassFreq  = k "lff" lff
    highpassFreq = k "hff" hff
    ampLevel     = k "a" a
    duration     = k "d" 5
    gate         = k "g" 1
    attack       = k "at" at
    release      = k "rl" 40

c4sio :: Double -> Double -> Double -> IO Int
c4sio f a rl =
  synthDef "c4sio"
  $ amp ampLevel
  $ env gate 15 release
  $ lmtr
  $ sinOsc AR (mce [freq, freq + 1]) 1
  where
    freq     = k "f" f
    ampLevel = k "a" a
    release  = k "rl" rl
    gate     = k "g" 1

--
-- SynthDefs for c5.hs
--

c5wr :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO Int
c5wr b p a fs ws lff hff =
  synthDef "c5wr"
  $ amp ampLevel
  $ env gate 15 40
  $ fvrb' 1 1 1
  $ hf highpassFreq
  $ lf lowpassFreq
  $ warp1 2 buffer pointer freqScale windowSize (-1) overlaps 0.0 4
  where
    buffer       = k "b" b
    pointer      = k "p" p
    ampLevel     = k "a" a
    freqScale    = k "fs" fs
    windowSize   = k "ws" ws
    lowpassFreq  = k "lff" lff
    highpassFreq = k "hff" hff
    overlaps     = k "ol" 1
    gate         = k "g" 1

c5wm :: Double -> Double -> Double -> Double -> Double -> IO Int
c5wm b a fs lff hff =
  let
    pointer    = mouseX KR 0 1 Linear 0.2
    windowSize = mouseY KR 0.05 6 Linear 0.2
  in
    synthDef "c5wm"
    $ pan
    $ amp ampLevel
    $ env gate 15 40
    $ hf highpassFreq
    $ lf lowpassFreq
    $ warp1 1 buffer pointer freqScale windowSize (-1) overlaps 0.0 4
  where
    buffer       = k "b" b
    ampLevel     = k "a" a
    freqScale    = k "fs" fs
    lowpassFreq  = k "lff" lff
    highpassFreq = k "hff" hff
    overlaps     = k "ol" 1
    gate         = k "g" 1

c5wm' :: Double -> Double -> Double -> Double -> Double -> UGen -> UGen -> UGen -> UGen -> IO Int
c5wm' b a fs lff hff xMin xMax yMin yMax =
  let
    pointer    = mouseX KR xMin xMax Linear 0.2
    windowSize = mouseY KR yMin yMax Linear 0.2
  in
  synthDef "c5wm"
  $ pan
  $ amp ampLevel
  $ env gate 15 40
  $ hf highpassFreq
  $ lf lowpassFreq
  $ warp1 1 buffer pointer freqScale windowSize (-1) overlaps 0.0 4
  where
    buffer       = k "b" b
    ampLevel     = k "a" a
    freqScale    = k "fs" fs
    lowpassFreq  = k "lff" lff
    highpassFreq = k "hff" hff
    overlaps     = k "ol" 1
    gate         = k "g" 1

c5pb :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO Int
c5pb b sp r lff hff a at rl =
  synthDef "c5pb"
  $ pan
  $ amp ampLevel
  $ env gate attack release
  $ hf highpassFreq
  $ lf lowpassFreq
  $ playBuf 1 AR buffer rate 1 startPos Loop RemoveSynth
  where
    buffer       = k "b" b
    startPos     = k "sp" sp
    rate         = k "r" r
    lowpassFreq  = k "lff" lff
    highpassFreq = k "hff" hff
    ampLevel     = k "a" a
    attack       = k "at" at
    release      = k "rl" rl
    gate         = k "g" 1

--
-- SynthDefs for c6.hs
--

c6w :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO Int
c6w b p a fs ws lff hff at rl =
  synthDef "c6w"
  $ amp ampLevel
  $ env gate attack release
  $ hf highpassFreq
  $ lf lowpassFreq
  $ warp1 2 buffer pointer freqScale windowSize (-1) overlaps 0.0 4
  where
    buffer       = k "b" b
    pointer      = k "p" p
    ampLevel     = k "a" a
    freqScale    = k "fs" fs
    windowSize   = k "ws" ws
    lowpassFreq  = k "lff" lff
    highpassFreq = k "hff" hff
    attack       = k "at" at
    release      = k "rl" rl
    overlaps     = k "ol" 1
    gate         = k "g" 1
