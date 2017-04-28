module Functions where

import Sound.SC3
import Sound.OSC
import Data.List
import Data.Maybe
import System.Environment
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

-- Create buffer & load file
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

-- Counter for available nodes
nodeCounter :: IORef Integer
nodeCounter = unsafePerformIO $ newIORef 2

-- Increment next available node
nn :: IO Integer
nn = do
  i <- readIORef nodeCounter
  writeIORef nodeCounter $ i+1
  return i

-- Get next available node
n :: IO Integer
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

recInit :: IO ()
recInit = withSC3
  $ sendBundle
  $ bundle immediately
  $ rec_init_m d_SC3_Recorder

recStart :: IO ()
recStart = withSC3
  $ sendMessage
  $ rec_begin_m d_SC3_Recorder

recStop :: IO ()
recStop = withSC3
  $ sendBundle
  $ bundle immediately
  $ rec_end_m d_SC3_Recorder
