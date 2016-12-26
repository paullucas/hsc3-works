module Functions where

import Sound.SC3
import Sound.OSC
import Data.List
import Data.Maybe

-- Sample Rate
samplerate :: Double
samplerate = 48000

-- Create group 1
g :: IO ()
g = withSC3 (send (g_new [(1, AddToTail, 0)]))

-- Kill group 1
k :: IO ()
k = withSC3 (send (n_free [1]))

-- Create group x
gx :: Int -> IO ()
gx x = withSC3 (send (g_new [(x, AddToTail, 0)]))

-- Kill group x
kx :: Int -> IO ()
kx x = withSC3 (send (n_free [x]))

-- For node x, set y to z
c :: Int -> String -> Double -> IO ()
c x y z = withSC3 (send (n_set1 x y z))

-- Audition x
o :: Audible e => e -> IO ()
o x = audition x

-- Audition mono signal to out 0 & 1
mono :: UGen -> IO ()
mono x = audition $ mce [out 0 x, out 1 x]

-- Create buffer & load file (fn)
rb :: Int -> String -> IO (Message)
rb bn fn =
  withSC3 (do
    async (b_allocRead bn fn 0 0))

-- Load samples in directory
sd :: [Char] -> [[Char]] -> [IO Message]
sd dn fl =
  map (\x -> withSC3 (do async (b_allocRead (fromJust (findIndex (x ==) fl)) (dn ++ x) 0 0))) fl

-- Convert midi note number to hz
m2h :: Floating a => a -> a
m2h n = 440.0 * (2.0 ** ((n - 69.0) / 12.0))

-- Convert hz to midi note number
h2m :: (Floating a, Integral b, RealFrac a) => a -> b
h2m f = round (69 + (12 * ((log (f * 0.0022727272727)) / (log 2))))

-- Ternary Operator
(?) :: Bool -> (t, t) -> t
a ? (b, c) = if a then b else c

loopLogic :: (Eq a, Num a) => a -> Loop
loopLogic x = (x == 1) ? (Loop, NoLoop)

tgrain :: UGen -> UGen -> UGen -> UGen -> UGen
tgrain buf rate cpos dur = tGrains 2 (impulse AR 4 0) buf rate cpos dur 0.5 1 1

tgrainv :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tgrainv buf rate cpos dur tamp = tGrains 2 (impulse AR 4 0) buf rate cpos dur 0.5 tamp 1

cpos :: UGen
cpos = (sinOsc KR 0.03 0) * 0.1

env :: UGen -> UGen -> UGen -> UGen -> UGen
env gate att rel input = input * envGen KR gate 1 0 1 RemoveSynth (envASR att 1 rel EnvLin)

lf :: UGen -> UGen -> UGen
lf lff input = lpf input lff

hf :: UGen -> UGen -> UGen
hf hff input = hpf input hff

fvrb :: UGen -> UGen
fvrb input = freeVerb input 0.5 1 1

fvrbv :: UGen -> UGen -> UGen -> UGen -> UGen
fvrbv m r d input = freeVerb input m r d

gvrb :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gvrb rs rt d ibw s dl er tl mrs input = gVerb input rs rt d ibw s dl er tl mrs

fout :: Num a => a -> a -> a
fout amp input = input * amp

onepole :: UGen -> UGen -> UGen
onepole coef input = onePole input coef

tg :: Int -> Double -> Double -> Double -> Double -> IO ()
tg n b r a at =
  withSC3 (do async (d_recv (synthdef "tg" (out 0 output)))
              ;send (s_new "tg" n AddToTail 1 []))
  where
    output :: UGen                                      
    output = fout amp
             $ env gate att rel
             $ tgrain buf rate cpos dur
    [buf, rate, amp, dur, gate, att, rel] = control_set [control KR "b" b
                                                        ,control KR "r" r
                                                        ,control KR "a" a
                                                        ,control KR "d" 5
                                                        ,control KR "g" 1
                                                        ,control KR "at" at
                                                        ,control KR "rl" 40]

tgr :: Int -> Double -> Double -> Double -> Double -> IO ()
tgr n b r a at =
  withSC3 (do async (d_recv (synthdef "tgr" (out 0 output)))
              ;send (s_new "tgr" n AddToTail 1 []))
  where
    output :: UGen                                      
    output = fout amp
             $ env gate att rel
             $ fvrb
             $ tgrain buf rate cpos dur
    [buf, rate, amp, dur, gate, att, rel] = control_set [control KR "b" b
                                                        ,control KR "r" r
                                                        ,control KR "a" a
                                                        ,control KR "d" 5
                                                        ,control KR "g" 1
                                                        ,control KR "at" at
                                                        ,control KR "rl" 40]

tgg :: Int -> Double -> Double -> Double -> Double -> IO ()
tgg n b r a at =
  withSC3 (do async (d_recv (synthdef "tgg" (out 0 (output))))
              ;send (s_new "tgg" n AddToTail 1 []))
  where
    output :: UGen                                      
    output = fout amp
             $ env gate att rel
             $ gvrb 0.5 1.0 1.0 0.5 15 1 0.7 0.5 300
             $ tgrainv buf rate cpos dur 0.75
    [buf, rate, amp, dur, gate, att, rel] = control_set [control KR "b" b
                                                        ,control KR "r" r
                                                        ,control KR "a" a
                                                        ,control KR "d" 5
                                                        ,control KR "g" 1
                                                        ,control KR "at" at
                                                        ,control KR "rl" 40]

so :: Int -> Double -> Double -> Double -> IO ()
so n f a at = withSC3 (do async (d_recv (synthdef "so" (out 0 output)))
                          ;send (s_new "so" n AddToTail 1 []))
  where
    output :: UGen                                      
    output = fout amp
             $ env gate att rel
             $ sinOsc AR freq 1
    [freq, amp, gate, att, rel] = control_set [control KR "f" f
                                              ,control KR "a" a
                                              ,control KR "g" 1
                                              ,control KR "at" at
                                              ,control KR "rl" 40]

sor :: Int -> Double -> Double -> Double -> IO ()
sor n f a at = withSC3 (do async (d_recv (synthdef "sor" (out 0 output)))
                           ;send (s_new "sor" n AddToTail 1 []))
  where
    output :: UGen                                      
    output = fout amp
             $ env gate att rel
             $ fvrbv 0.33 0.5 0.5
             $ sinOsc AR freq 1
    [freq, amp, gate, att, rel] = control_set [control KR "f" f
                                              ,control KR "a" a
                                              ,control KR "g" 1
                                              ,control KR "at" at
                                              ,control KR "rl" 40]


-- SynthDefs for c1.hs

c1tg :: Int -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
c1tg n b d r lff hff a rl =
  withSC3 (do async (d_recv (synthdef "c1tg" (out 0 (output))))
              ;send (s_new "c1tg" n AddToTail 1 []))
  where
    output :: UGen                                      
    output = fout amp
             $ env gate 25 rel
             $ gvrb 15 6 0.5 0.5 20 0 0.7 0.5 300
             $ hf highpass
             $ lf lowpass
             $ tgrainv buf rate cpos dur 0.75
    [buf, dur, rate, lowpass, highpass, amp, rel, gate] = control_set [control KR "b" b
                                                                      ,control KR "d" d
                                                                      ,control KR "r" r
                                                                      ,control KR "lff" lff
                                                                      ,control KR "hff" hff
                                                                      ,control KR "a" a
                                                                      ,control KR "rl" rl
                                                                      ,control KR "g" 1]

c1tgl :: Int -> Double -> Double -> IO ()
c1tgl n b r = 
  withSC3 (do async (d_recv (synthdef "c1tgl" (out 0 (output))))
              ;send (s_new "c1tgl" n AddToTail 1 []))
  where
    output :: UGen                                      
    output = fout 1.8
             $ env gate 10 40
             $ gvrb 15 6 0.5 0.5 20 0.2 0.7 0.5 300
             $ lf 1000
             $ tGrains 1 (impulse AR 0.15 0) buf rate ((sinOsc KR 0.06 0) * 0.1) 7 0.5 1.5 1
    [buf, rate, gate] = control_set [control KR "b" b
                                    ,control KR "r" r
                                    ,control KR "g" 1]

c1sio :: Int -> Double -> Double -> Double -> IO ()
c1sio n f a rl =
  withSC3 (do async (d_recv (synthdef "c1sio" (out 0 (output))))
              ;send (s_new "c1sio" n AddToTail 1 []))
  where
    output :: UGen                                      
    output = fout amp
             $ env gate 15 rel
             $ sinOsc AR (mce [freq, freq + 1]) 1
    [freq, amp, rel, gate] = control_set [control KR "f" f
                                         ,control KR "a" a
                                         ,control KR "rl" rl
                                         ,control KR "g" 1]

-- SynthDefs for c2.hs

c2s1pb :: Int -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
c2s1pb n b sp r lff hff a at rl =
  withSC3 (do async (d_recv (synthdef "c2s1pb" (out 0 (output))))
              ;send (s_new "c2s1pb" n AddToTail 1 []))
  where
    output :: UGen                                      
    output = fout amp
             $ env gate att rel
             $ hf highpass
             $ lf lowpass
             $ playBuf 2 AR buf rate 1 spos Loop RemoveSynth
    [buf, spos, rate, lowpass, highpass, amp, att, rel, gate] = control_set [control KR "b" b
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
  withSC3 (do async (d_recv (synthdef "c2s2pb" (out 0 (output))))
              ;send (s_new "c2s2pb" n AddToTail 1 []))
  where
    output :: UGen                                     
    output = fvrb 
             $ env gate att rel
             $ hf highpass
             $ lf lowpass
             $ fout amp
             $ playBuf 2 AR buf rate 1 spos (loopLogic loop) RemoveSynth
    [buf, spos, rate, lowpass, highpass, amp, att, rel, gate, loop] = control_set [control KR "b" b
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
  withSC3 (do async (d_recv (synthdef "c2s2pbm" (out 0 (output))))
              ;send (s_new "c2s2pbm" n AddToTail 1 []))
  where
    output :: UGen                                      
    output = fvrb 
             $ env gate att rel
             $ hf highpass
             $ lf lowpass
             $ fout amp
             $ playBuf 1 AR buf rate 1 spos Loop RemoveSynth
    [buf, spos, rate, lowpass, highpass, amp, att, rel, gate] = control_set [control KR "b" b
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
  withSC3 (do async (d_recv (synthdef "c2s2pg" (out 0 output)))
              ;send (s_new "c2s2pg" n AddToTail 1 []))
  where
    output :: UGen                                      
    output = fvrb 
             $ env gate att rel
             $ hf highpass
             $ lf lowpass
             $ tGrains 2 (impulse AR trig 0) buf rate cpos dur 0 amp 1
    [buf, dur, cpos, rate, lowpass, highpass, amp, att, rel, trig, gate] = control_set [control KR "b" b
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

