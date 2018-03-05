module Works.C6 where

import           Functions
import           SynthDefs.C6 (c6s, c6w, c6wr)

main :: IO ()
main = do
  -- sdr "/Users/paul/Producing/april20th-2017/samples/" "f" [1..4]
  sdr "/home/plll/Producing/april20th-2017/samples/" "f" [1..4]
  g
  s1 <- c6w 0 0.1 2 0.08 10 2000 50 20 40
  c s1 "ol" 2
  s2 <- c6w 1 0.1 0.8 0.2 9 2000 80 15 40
  s3 <- c6w 2 0.5 1 0.4 8 4000 200 25 40
  c s1 "ol" 4
  c s2 "ol" 3
  c s3 "ol" 5
  c s1 "fs" 0.06
  c s2 "fs" 0.14
  c s3 "fs" 0.17
  s4 <- c6wr 3 0.0 2 0.2 15 4000 100
  s5 <- c6s 90 0.4 40
  c s2 "fs" 0.18
  c s1 "fs" 0.09
  c s3 "fs" 0.19
  s6 <- c6s 3000 0.004 30
  c s6 "g" 0
  c s1 "g" 0
  c s2 "fs" 0.16
  c s4 "g" 0
  c s2 "g" 0
  c s3 "ol" 1
  c s3 "g" 0
  c s5 "g" 0
  f
