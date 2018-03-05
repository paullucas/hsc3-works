module UGens where

import           Sound.SC3.Common.Envelope
import           Sound.SC3.UGen
import qualified Sound.SC3.UGen.Bindings.DB as U

--
-- UGen Type Aliases
--
type Gate_ = UGen

type Att_ = UGen

type Rel_ = UGen

type In_ = UGen

type Db_ = UGen

type Freq_ = UGen

type Lvl_ = UGen

type Dur_ = UGen

type RoomSize_ = UGen

type RevTime_ = UGen

type Damp_ = UGen

type InBW_ = UGen

type Spread_ = UGen

type DryLvl_ = UGen

type EarlyRefLvl_ = UGen

type TailLvl_ = UGen

type MaxRoomSize_ = UGen

type Mix_ = UGen

type BufN_ = UGen

type SampleRate_ = UGen

type Trig_ = UGen

type StartPos_ = UGen

type Phase_ = UGen

--
-- UGen Wrapper Fns
--
k :: String -> Double -> UGen
k = control KR

env :: Gate_ -> Att_ -> Rel_ -> In_ -> UGen
env gate attack release input =
  input * U.envGen KR gate 1 0 1 RemoveSynth (envASR attack 1 release EnvLin)

o :: Db_ -> In_ -> UGen
o db i = U.out 0 $ i * db

lpf :: Freq_ -> In_ -> UGen
lpf f i = U.lpf i f

hpf :: Freq_ -> In_ -> UGen
hpf f i = U.hpf i f

limiter :: Lvl_ -> Dur_ -> In_ -> UGen
limiter l d i = U.limiter i l d

freeVerb :: Mix_ -> RoomSize_ -> Damp_ -> In_ -> UGen
freeVerb mx room damp i = U.freeVerb i mx room damp

gVerb ::
     RoomSize_
  -> RevTime_
  -> Damp_
  -> InBW_
  -> Spread_
  -> DryLvl_
  -> EarlyRefLvl_
  -> TailLvl_
  -> MaxRoomSize_
  -> In_
  -> UGen
gVerb rs rt d bw sp dl rl tl mrs i = U.gVerb i rs rt d bw sp dl rl tl mrs

pbuf :: BufN_ -> SampleRate_ -> Trig_ -> StartPos_ -> UGen
pbuf buffer rate trigger startPos =
  U.playBuf 2 AR buffer rate trigger startPos Loop RemoveSynth

sinO :: Freq_ -> Phase_ -> UGen
sinO = U.sinOsc AR

sinO' :: Rate -> Freq_ -> Phase_ -> UGen
sinO' = U.sinOsc
