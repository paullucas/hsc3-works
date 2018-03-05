module Works.C8 where

import           Functions
import           SynthDefs.C8 (c8p, c8w)

main :: IO ()
main = do
  sdr "/home/plll/Producing/june18th-2017/dup/" "f" [1..8]
  g
  p1 <- c8w 1 0.8 1.2 0.1 4 2000 50
  p2 <- c8w 0 0.8 1.2 0.05 5 800 50
  p3 <- c8p 4 0.25 4 0.0 1 1000 50 20 40
  c' p3 "t" 0.03 "a" 1.2
  c  p3 "t" 0.028
  c' p3 "t" 4 "g" 0
  c' p1 "p" 0.5 "ws" 7
  c' p2 "p" 0.1 "ws" 8
  p4 <- c8w 2 0.1 1.5 0.08 10 3000 50
  p5 <- c8w 5 0.1 1.5 0.35 10 6000 50
  c' p4 "fs" 0.5 "a" 1.2
  c' p4 "fs" 0.8 "ws" 12
  c  p4 "fs" 1.7
  c  p4 "fs" 0.6
  c  p4 "fs" 0.45
  c  p4 "fs" 0.5
  c  p1 "fs" 0.09
  c  p2 "fs" 0.09
  c  p5 "g" 0
  c  p4 "g" 0
  c  p1 "fs" 0.06
  c  p2 "fs" 0.06
  c  p2 "g" 0
  c  p1 "g" 0
  f
