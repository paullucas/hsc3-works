-- http://hackage.haskell.org/package/conductive-hsc3/src/Sound/Conductive/Synths.hs

-- defineSynth func synthName = withSC3 (do async (d_recv (synthdef synthName (out 0 func))))
-- playSynth ps synthName = withSC3 (do async (s_new synthName (-1) AddToTail 1 ps))
-- sDef = defineSynth (sinOsc AR (control KR "freq" 100) 0 * (control KR "amp" 0.1)) "sTest"
-- sTest freq amp = playSynth [("freq", freq),("amp", amp)] "sTest"

-- defineSynth :: UGen -> String -> IO OSC
-- defineSynth func synthName = withSC3 i where
--     o = out 0 func
--     i fd = do async fd (d_recv (synthdef synthName o))

-- playSynth :: [(String, Double)] -> String -> IO ()
-- playSynth ps synthName = withSC3 (\fd -> send fd (s_new synthName (-1) AddToTail 1 ps))

-- sDef = defineSynth (sinOsc AR f 0 * a) "sTest" where
--    f = control KR "freq" 100
--    a = control KR "amp" 0.1

-- sTest freq amp = playSynth ps "sTest" where
--   ps = [("freq", freq)
--        ,("amp", amp)
--        ]
