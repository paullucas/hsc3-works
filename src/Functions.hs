module Functions where

import Data.Int
import Data.List
import Data.Maybe
import Data.IORef
import Sound.OSC
import Sound.SC3.Server.Graphdef (Graphdef, encode_graphdef)
import Sound.SC3.Server.Synthdef (Synthdef, synthdef, synthdef_to_graphdef)
import Sound.SC3.Server.Recorder (SC3_Recorder, rec_init_m, rec_begin_m, rec_end_m, default_SC3_Recorder, rec_buf_frames)
import Sound.SC3.UGen (UGen)
import System.Process
import System.IO.Unsafe

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

-- Communicate with SuperCollider
wsc :: Connection UDP a -> IO a
wsc = withTransport (openUDP "127.0.0.1" 57110)

-- Create a message
msg :: Address_Pattern -> [Datum] -> Message
msg = message

-- Send a message
sm :: SendOSC m => Message -> m ()
sm = sendMessage

-- Send a message & wait for response
smA :: (SendOSC m, RecvOSC m) => Message -> m Message
smA m = sm m >> waitReply "/done"

-- Create group on node 1
g :: IO ()
g = wsc . sm $ msg "/g_new" [int32 0, int32 1, int32 0]

-- Create group on node n
g' :: Int32 -> IO ()
g' node = wsc . sm $ msg "/g_new" [int32 node, int32 1, int32 0]

-- Free group on node 1
f :: IO ()
f = wsc . sm $ msg "/n_free" [int32 1]

-- Free group on node n
f' :: Int32 -> IO ()
f' node = wsc . sm $ msg "/n_free" [int32 node]

-- Receive synth definition file
d_recv :: Graphdef -> Message
d_recv graphdef = msg "/d_recv" [Blob (encode_graphdef graphdef)]

-- Create a new synth
s_new :: String -> Int32 -> Message
s_new name node = msg "/s_new" [ASCII_String (ascii name), int32 node, int32 1, int32 1]

-- Create & instantiate synthdef
sd :: Num b => String -> UGen -> IO b
sd name ugen = do
  node <- nn
  wsc $ do
    smA . d_recv . synthdef_to_graphdef $ synthdef name ugen
    sm $ s_new name (fromIntegral node)
  return (fromIntegral node)

-- Set a node's control value
n_set :: Integral n => n -> String -> Float -> Message
n_set node key value = msg "/n_set" [int32 node , ASCII_String (ascii key) , Float value]

-- Send control messages to node
c :: Integral n => n -> String -> Float -> IO ()
c node key value = wsc . sm $ n_set node key value

-- Send 2 control messages to node
c' :: Integral n => n -> String -> Float -> String -> Float -> IO ()
c' node k1 v1 k2 v2 = wsc $ do sm $ n_set node k1 v1
                               sm $ n_set node k2 v2

-- Get buffer info
b_query :: Integral n => n -> Message
b_query buffer = msg "/b_query" [int32 buffer]

-- Query a buffer & print response
bq :: Integral n => n -> IO ()
bq buffer = wsc $ do
  sm $ b_query buffer
  res <- waitReply "/b_info"
  liftIO $ print res

-- Get node info
n_query :: Integral n => n -> Message
n_query node = msg "/n_query" [int32 node]

-- Query a node & print response
nq node = wsc $ do
  sm $ n_query node
  res <- waitReply "/n_info"
  liftIO $ print res

-- Amplify input
amp :: Num a => a -> a -> a
amp ampLevel input = input * ampLevel

-- Allocate buffer
b_alloc :: (Integral n2, Integral n1, Integral n) => n2 -> n1 -> n -> Message
b_alloc buffer frames channels = msg "/b_alloc" [int32 buffer, int32 frames, int32 channels]

-- Allocate buffer & read sound
b_allocRead :: Integral n => n -> String -> Message
b_allocRead buffer path = msg "/b_allocRead" [int32 buffer, string path, int32 0, int32 0]

-- Load samples in directory
sdr :: Show a => String -> String -> [a] -> IO ()
sdr path suffix numbers =
  sequence_ $ map loadFile files
  where
    files         = map (\x -> (show x) ++ suffix ++ ".wav") numbers
    loadFile file = let filePath = path ++ file
                        index    = fromJust $ findIndex (file ==) files
                    in  wsc $ smA $ b_allocRead index filePath

-- Default recording settings
dsr :: SC3_Recorder
dsr = default_SC3_Recorder {rec_buf_frames = 65536}
-- dsr = default_SC3_Recorder {rec_buf_frames = 65536, rec_buf_id = 10}

-- Initialize recording
rI :: IO ()
rI = wsc . sendBundle . bundle immediately $ rec_init_m dsr

-- Start recording
rS :: IO ()
rS = wsc . sm $ rec_begin_m dsr

-- End recording
rE :: IO ()
rE = wsc . sendBundle . bundle immediately $ rec_end_m dsr

-- Start SuperCollider
boot :: IO ()
boot = do
    system "scsynth -u 57110 &"
    system "sleep 5"
    system "/bin/bash -c \"[ \"$(uname -s)\" == \"Linux\" ] && jack_connect SuperCollider:out_1 system:playback_1\""
    system "/bin/bash -c \"[ \"$(uname -s)\" == \"Linux\" ] && jack_connect SuperCollider:out_2 system:playback_2\""
    putStrLn "Ok!"
