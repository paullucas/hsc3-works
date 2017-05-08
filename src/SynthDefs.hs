module SynthDefs where

import Functions
import UGens
import Sound.SC3.UGen
import Sound.SC3.UGen.Bindings

tg :: Double -> Double -> Double -> Double -> IO Int
tg b r a at =
  sd "tg"
  $ amp ampLevel
  $ env gate attack release
  $ tgrain buffer rate cpos duration
  where
    buffer   = k "b" b
    rate     = k "r" r
    ampLevel = k "a" a
    duration = k "d" 5
    gate     = k "g" 1
    attack   = k "at" at
    release  = k "rl" 40

tgr :: Double -> Double -> Double -> Double -> IO Int
tgr b r a at =
  sd "tgr"
  $ amp ampLevel
  $ env gate attack release
  $ fvrb
  $ tgrain buffer rate cpos duration
  where
    buffer   = k "b" b
    rate     = k "r" r
    ampLevel = k "a" a
    duration = k "d" 5
    gate     = k "g" 1
    attack   = k "at" at
    release  = k "rl" 40

tgrf :: Double -> Double -> Double -> Double -> Double -> Double -> IO Int
tgrf b r lff hff a at =
  sd "tgrf"
  $ amp ampLevel
  $ env gate attack release
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

tgg :: Double -> Double -> Double -> Double -> IO Int
tgg b r a at =
  sd "tgg"
  $ amp ampLevel
  $ env gate attack release
  $ gvrb 0.5 1.0 1.0 0.5 15 1 0.7 0.5 300
  $ tgrain' buffer rate cpos duration 0.75
  where
    buffer       = k "b" b
    rate         = k "r" r
    ampLevel     = k "a" a
    duration     = k "d" 5
    gate         = k "g" 1
    attack       = k "at" at
    release      = k "rl" 40

so :: Double -> Double -> Double -> IO Int
so f a at =
  sd "so"
  $ amp ampLevel
  $ env gate attack release
  $ sinOsc AR freq 1
  where
    freq     = k "f" f
    ampLevel = k "a" a
    gate     = k "g" 1
    attack   = k "at" at
    release  = k "rl" 40

sor :: Double -> Double -> Double -> IO Int
sor f a at =
  sd "sor"
  $ amp ampLevel
  $ env gate attack release
  $ fvrb' 0.33 0.5 0.5
  $ sinOsc AR freq 1
  where
    freq     = k "f" f
    ampLevel = k "a" a
    gate     = k "g" 1
    attack   = k "at" at
    release  = k "rl" 40

--
-- SynthDefs for c1.hs
--

c1tg :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO Int
c1tg b d r lff hff a rl =
  sd "c1tg"
  $ amp ampLevel
  $ env gate 25 release
  $ gvrb 15 6 0.5 0.5 20 0 0.7 0.5 300
  $ hf highpassFreq
  $ lf lowpassFreq
  $ tgrain' buffer rate cpos duration 0.75
  where
    buffer       = k "b" b
    duration     = k "d" d
    rate         = k "r" r
    lowpassFreq  = k "lff" lff
    highpassFreq = k "hff" hff
    ampLevel     = k "a" a
    release      = k "rl" rl
    gate         = k "g" 1

c1tgl :: Double -> Double -> Double -> IO Int
c1tgl b r a =
  sd "c1tgl"
  $ amp ampLevel
  $ env gate 10 40
  $ hf 50
  $ gvrb 15 6 0.5 0.5 20 0.2 0.7 0.5 300
  $ lf 1000
  $ tGrains 1 (impulse AR 0.15 0) buffer rate ((sinOsc KR 0.06 0) * 0.1) 7 0.5 1 1
  where
    buffer   = k "b" b
    rate     = k "r" r
    gate     = k "g" 1
    ampLevel = k "a" a

c1sio :: Double -> Double -> Double -> IO Int
c1sio f a rl =
  sd "c1sio"
  $ amp ampLevel
  $ env gate 15 release
  $ sinOsc AR (mce [freq, freq + 1]) 1
  where
    freq     = k "f" f
    ampLevel = k "a" a
    release  = k "rl" rl
    gate     = k "g" 1

--
-- SynthDefs for c2.hs
--

c2s1pb :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO Int
c2s1pb b sp r lff hff a at rl =
  sd "c2s1pb"
  $ amp ampLevel
  $ env gate attack release
  $ hf highpassFreq
  $ lf lowpassFreq
  $ playBuf 2 AR buffer rate 1 startPos Loop RemoveSynth
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

c2s2pb :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO Int
c2s2pb b sp r lff hff a at rl =
  sd "c2s2pb"
  $ fvrb 
  $ env gate attack release
  $ hf highpassFreq
  $ lf lowpassFreq
  $ amp ampLevel
  $ playBuf 2 AR buffer rate 1 startPos (loop' loop) RemoveSynth
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
    loop         = k "l" 1

c2s2pbm :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO Int
c2s2pbm b sp r lff hff a at rl =
  sd "c2s2pbm"
  $ fvrb 
  $ env gate attack release
  $ hf highpassFreq
  $ lf lowpassFreq
  $ amp ampLevel
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

c2s2pg :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO Int
c2s2pg b d cp r lff hff a at rl t =
  sd "c2s2pg"
  $ fvrb 
  $ env gate attack release
  $ hf highpassFreq
  $ lf lowpassFreq
  $ tGrains 2 (impulse AR trigger 0) buffer rate cpos duration 0 ampLevel 1
  where
    buffer       = k "b" b
    duration     = k "d" d
    centerPos    = k "cp" cp
    rate         = k "r" r
    lowpassFreq  = k "lff" lff
    highpassFreq = k "hff" hff
    ampLevel     = k "a" a
    attack       = k "at" at
    release      = k "rl" rl
    trigger      = k "t" t
    gate         = k "g" 1

--
-- SynthDefs for c3.hs
--

c3s2wlf :: Double -> Double -> Double -> Double -> Double -> Double -> IO Int
c3s2wlf b p a lff fs ws =
  sd "c3s2wlf"
  $ amp ampLevel
  $ env gate 15 40
  $ lf lowpassFreq
  $ warp1 1 buffer pointer freqScale windowSize (-1) overlaps 0.0 4
  where
    buffer      = k "b" b
    pointer     = k "p" p
    ampLevel    = k "a" a
    lowpassFreq = k "lff" lff
    freqScale   = k "fs" fs
    windowSize  = k "ws" ws
    overlaps    = k "ol" 1
    gate        = k "g" 1

c3s2whf :: Double -> Double -> Double -> Double -> Double -> Double -> IO Int
c3s2whf b p a hff fs ws =
  sd "c3s2whf"
  $ amp ampLevel
  $ env gate 15 40
  $ hf highpassFreq
  $ warp1 1 buffer pointer freqScale windowSize (-1) overlaps 0.0 4
  where
    buffer       = k "b" b
    pointer      = k "p" p
    ampLevel     = k "a" a
    highpassFreq = k "hff" hff
    freqScale    = k "fs" fs
    windowSize   = k "ws" ws
    overlaps     = k "ol" 1
    gate         = k "g" 1

c3s2wr :: Double -> Double -> Double -> Double -> Double -> IO Int
c3s2wr b p a fs ws =
  sd "c3s2wr"
  $ amp ampLevel
  $ env gate 15 40
  $ fvrb' 1 1 1
  $ warp1 1 buffer pointer freqScale windowSize (-1) overlaps 0.0 4
  where
    buffer     = k "b" b
    pointer    = k "p" p
    ampLevel   = k "a" a
    freqScale  = k "fs" fs
    windowSize = k "ws" ws
    overlaps   = k "ol" 1
    gate       = k "g" 1

--
-- SynthDefs for c4.hs
--

c4tgr :: Double -> Double -> Double -> Double -> IO Int
c4tgr b r a at =
  sd "c4tgr"
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
  sd "c4tgrf"
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
  sd "c4sio"
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
  sd "c5wr"
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
    sd "c5wm"
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
  sd "c5wm"
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
  sd "c5pb"
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
  sd "c6w"
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
