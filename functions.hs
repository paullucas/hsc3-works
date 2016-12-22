module Functions where

import Sound.SC3
import Sound.OSC
import Data.List
import Data.Maybe

-- Create group 1
g = withSC3 (send (g_new [(1, AddToTail, 0)]))

-- Kill group 1
k = withSC3 (send (n_free [1]))

-- Create group x
gx x = withSC3 (send (g_new [(x, AddToTail, 0)]))

-- Kill group x
kx x = withSC3 (send (n_free [x]))

-- For node x, set y to z
c x y z = withSC3 (send (n_set1 x y z))

-- Audition x
o x = audition x

-- Audition mono signal to out 0 & 1
mono x = audition $ mce [out 0 x, out 1 x]

-- Create buffer & load file (fn)
rb bn fn =
  withSC3 (do
    async (b_allocRead bn fn 0 0))

-- Convert midi note number to hz
m2h n = 440.0 * (2.0 ** ((n - 69.0) / 12.0))

-- Convert hz to midi note number
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

so :: Int -> Double -> Double -> IO ()
so n f a = withSC3 (do async (d_recv (synthdef "so" (out 0 output)))
                       ;send (s_new "so" n AddToTail 1 []))
  where [freq, amp, gate] = control_set [control KR "f" f
                                        ,control KR "a" a
                                        ,control KR "g" 1]
        synth :: UGen
        synth = sinOsc AR freq 1
        env :: UGen
        env = envGen KR gate 1 0 1 RemoveSynth (envASR 25 1 25 EnvLin)
        output :: UGen
        output = (synth * env) * amp

sor :: Int -> Double -> Double -> IO ()
sor n f a = withSC3 (do async (d_recv (synthdef "sor" (out 0 output)))
                        ;send (s_new "sor" n AddToTail 1 []))
  where [freq, amp, gate] = control_set [control KR "f" f
                                        ,control KR "a" a
                                        ,control KR "g" 1]
        synth :: UGen
        synth = sinOsc AR freq 1
        rvrb :: UGen
        rvrb = freeVerb synth 0.33 0.5 0.5
        env :: UGen
        env = envGen KR gate 1 0 1 RemoveSynth (envASR 25 1 25 EnvLin)
        output :: UGen
        output = (rvrb * env) * amp
