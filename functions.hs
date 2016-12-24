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

-- SynthDefs

tg :: Int -> Double -> Double -> Double -> Double -> IO ()
tg n b r a at =
  withSC3 (do async (d_recv (synthdef "tg" (out 0 output)))
              ;send (s_new "tg" n AddToTail 1 []))
  where [buf, rate, amp, dur, gate, att, rel] = control_set [control KR "b" b
                                                            ,control KR "r" r
                                                            ,control KR "a" a
                                                            ,control KR "d" 5
                                                            ,control KR "g" 1
                                                            ,control KR "at" at
                                                            ,control KR "rl" 40]
        cpos :: UGen
        cpos = (sinOsc KR 0.03 0) * 0.1
        synth :: UGen
        synth = tGrains 2 (impulse AR 4 0) buf rate cpos dur 0.5 1 1
        env :: UGen
        env = envGen KR gate 1 0 1 RemoveSynth (envASR att 1 rel EnvLin)
        output :: UGen
        output = (synth * env) * amp

tgr :: Int -> Double -> Double -> Double -> Double -> IO ()
tgr n b r a at =
  withSC3 (do async (d_recv (synthdef "tgr" (out 0 output)))
              ;send (s_new "tgr" n AddToTail 1 []))
  where [buf, rate, amp, dur, gate, att, rel] = control_set [control KR "b" b
                                                            ,control KR "r" r
                                                            ,control KR "a" a
                                                            ,control KR "d" 5
                                                            ,control KR "g" 1
                                                            ,control KR "at" at
                                                            ,control KR "rl" 40]
        cpos :: UGen
        cpos = (sinOsc KR 0.03 0) * 0.1
        synth :: UGen
        synth = tGrains 2 (impulse AR 4 0) buf rate cpos dur 0.5 1 1
        rvrb :: UGen
        rvrb = freeVerb synth 0.5 1 1
        env :: UGen
        env = envGen KR gate 1 0 1 RemoveSynth (envASR att 1 rel EnvLin)
        output :: UGen
        output = (rvrb * env) * amp

tgg :: Int -> Double -> Double -> Double -> Double -> IO ()
tgg n b r a at =
  withSC3 (do async (d_recv (synthdef "tgg" (out 0 (output))))
              ;send (s_new "tgg" n AddToTail 1 []))
  where [buf, rate, amp, dur, gate, att, rel] = control_set [control KR "b" b
                                                            ,control KR "r" r
                                                            ,control KR "a" a
                                                            ,control KR "d" 5
                                                            ,control KR "g" 1
                                                            ,control KR "at" at
                                                            ,control KR "rl" 40]
        cpos :: UGen
        cpos = (sinOsc KR 0.03 0) * 0.1
        synth :: UGen
        synth = tGrains 2 (impulse AR 4 0) buf rate cpos dur 0.5 0.75 1
        gvrb :: UGen
        gvrb = gVerb synth 0.5 1.0 1.0 0.5 15 1 0.7 0.5 300 
        env :: UGen
        env = envGen KR gate 1 0 1 RemoveSynth (envASR att 1 rel EnvLin)
        output :: UGen
        output = (gvrb * env) * amp

so :: Int -> Double -> Double -> Double -> IO ()
so n f a at = withSC3 (do async (d_recv (synthdef "so" (out 0 output)))
                          ;send (s_new "so" n AddToTail 1 []))
  where [freq, amp, gate, att, rel] = control_set [control KR "f" f
                                                  ,control KR "a" a
                                                  ,control KR "g" 1
                                                  ,control KR "at" at
                                                  ,control KR "rl" 40]
        synth :: UGen
        synth = sinOsc AR freq 1
        env :: UGen
        env = envGen KR gate 1 0 1 RemoveSynth (envASR att 1 rel EnvLin)
        output :: UGen
        output = (synth * env) * amp

sor :: Int -> Double -> Double -> Double -> IO ()
sor n f a at = withSC3 (do async (d_recv (synthdef "sor" (out 0 output)))
                           ;send (s_new "sor" n AddToTail 1 []))
  where [freq, amp, gate, att, rel] = control_set [control KR "f" f
                                                  ,control KR "a" a
                                                  ,control KR "g" 1
                                                  ,control KR "at" at
                                                  ,control KR "rl" 40]
        synth :: UGen
        synth = sinOsc AR freq 1
        rvrb :: UGen
        rvrb = freeVerb synth 0.33 0.5 0.5
        env :: UGen
        env = envGen KR gate 1 0 1 RemoveSynth (envASR att 1 rel EnvLin)
        output :: UGen
        output = (rvrb * env) * amp

-- SynthDefs for c1.hs

c1tg :: Int -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
c1tg n b d r lff hff a rl =
  withSC3 (do async (d_recv (synthdef "c1tg" (out 0 (output))))
              ;send (s_new "c1tg" n AddToTail 1 []))
  where [buf, dur, rate, lowpass, highpass, amp, rel, gate] = control_set [control KR "b" b
                                                                          ,control KR "d" d
                                                                          ,control KR "r" r
                                                                          ,control KR "lff" lff
                                                                          ,control KR "hff" hff
                                                                          ,control KR "a" a
                                                                          ,control KR "rl" rl
                                                                          ,control KR "g" 1]
        cpos :: UGen
        cpos = (sinOsc KR 0.03 0) * 0.1
        synth :: UGen
        synth = tGrains 1 (impulse AR 4 0) buf rate cpos dur 0.5 0.75 1
        lf :: UGen
        lf = lpf synth lowpass
        hf :: UGen
        hf = hpf lf highpass
        gvrb :: UGen
        gvrb = gVerb hf 15 6 0.5 0.5 20 0 0.7 0.5 300
        env :: UGen
        env = envGen KR gate 1 0 1 RemoveSynth (envASR 25 1 rel EnvLin)
        output :: UGen
        output = (gvrb * env) * amp

c1tgl :: Int -> Double -> Double -> IO ()
c1tgl n b r = 
  withSC3 (do async (d_recv (synthdef "c1tgl" (out 0 (output))))
              ;send (s_new "c1tgl" n AddToTail 1 []))
  where [buf, rate, gate] = control_set [control KR "b" b
                                        ,control KR "r" r
                                        ,control KR "g" 1]
        cpos :: UGen
        cpos = (sinOsc KR 0.06 0) * 0.1
        synth :: UGen
        synth = tGrains 1 (impulse AR 0.15 0) buf rate cpos 7 0.5 1.5 1
        lf :: UGen
        lf = lpf synth 1000
        gvrb :: UGen
        gvrb = gVerb lf 15 6 0.5 0.5 20 0.2 0.7 0.5 300
        env :: UGen
        env = envGen KR gate 1 0 1 RemoveSynth (envASR 10 1 40 EnvLin)
        output :: UGen
        output = (gvrb * env) * 1.8

c1sio :: Int -> Double -> Double -> Double -> IO ()
c1sio n f a rl =
  withSC3 (do async (d_recv (synthdef "c1sio" (out 0 (output))))
              ;send (s_new "c1sio" n AddToTail 1 []))
  where [freq, amp, rel, gate] = control_set [control KR "f" f
                                             ,control KR "a" a
                                             ,control KR "rl" rl
                                             ,control KR "g" 1]
        synth :: UGen
        synth = sinOsc AR (mce [freq, freq + 1]) 1
        env :: UGen
        env = envGen KR gate 1 0 1 RemoveSynth (envASR 15 1 rel EnvLin)
        output :: UGen
        output = (synth * env) * amp

-- SynthDefs for c2.hs

c2s1pb n b sp r lff hff a at rl =
  withSC3 (do async (d_recv (synthdef "c2s1pb" (out 0 (output))))
              ;send (s_new "c2s1pb" n AddToTail 1 []))
  where [buf, spos, rate, lowpass, highpass, amp, att, rel, gate] = control_set [control KR "b" b
                                                                                ,control KR "sp" sp
                                                                                ,control KR "r" r
                                                                                ,control KR "lff" lff
                                                                                ,control KR "hff" hff
                                                                                ,control KR "a" a
                                                                                ,control KR "at" at
                                                                                ,control KR "rl" rl
                                                                                ,control KR "g" 1]
        synth :: UGen
        synth = playBuf 2 AR buf rate 1 spos Loop RemoveSynth
        lf :: UGen
        lf = lpf synth lowpass
        hf :: UGen
        hf = hpf lf highpass
        env :: UGen
        env = envGen KR gate 1 0 1 RemoveSynth (envASR att 1 rel EnvLin)
        output :: UGen
        output = (hf * env) * amp
