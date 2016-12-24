import Sound.SC3
import Sound.OSC

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

gvrb :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
gvrb rs rt d ibw s dl er tl mrs input = gVerb input rs rt d ibw s dl er tl mrs

fout :: Num a => a -> a -> a
fout amp input = input * amp

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
        output :: UGen                                      
        output = fout amp
                 $ env gate att rel
                 $ tgrain buf rate cpos dur


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
        output :: UGen                                      
        output = fout amp
                 $ env gate att rel
                 $ fvrb
                 $ tgrain buf rate cpos dur

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
        output :: UGen                                      
        output = fout amp
                 $ env gate att rel
                 $ gvrb 0.5 1.0 1.0 0.5 15 1 0.7 0.5 300
                 $ tgrainv buf rate cpos dur 0.75
