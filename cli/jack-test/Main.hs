{-# LANGUAGE TupleSections #-}
module Main (main) where

import Control.Applicative ((<$>))
import Data.Default (def)

import qualified Sound.MIDI.Controller as MC
import qualified Sound.MIDI.Message as M
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V

import Jebediah.JACK hiding (at)
import qualified Jebediah.JACK as J
import Jebediah.MIDI.Messages
import Jebediah.MIDI.Instrument
import Jebediah.MIDI.Nord.Electro4

targets :: [PortName]
targets = PortName <$> [ "Bass-Station-II:midi/capture_1"
                       , "Nord-Electro-4:midi/capture_1"
                       , "MIDISPORT-2x2-Anniv:midi/capture_1"
                       , "Blofeld:midi/capture_1"
                       , "midi-monitor:input"
                       ]

nOn = toChannel 1 $ noteOn 60 60
nOff = toChannel 1 $ noteOff 60 60
cC c = toChannel 1 . controlChange c

cfg :: Config
cfg = def { beatsPerMinute = 120
          , beatsPerMeasure = 2
          , targetPorts = targets
          }

at :: Int -> Int -> Int -> EventTime
at m b s = J.at cfg (Measure m) (Beat b) (Subdivision s)

at' m b s evts = ((at m b s),) <$> evts

eL :: MIDIEventList
eL = fromPairList $ (at' 1 1 1 (toChannel 1 <$> drawbars Upper "808")) ++
                  [ (at 1 1 1, nOn) ] ++
                  ( at' 1 2 1 (toChannel 1 <$> drawbars Upper "012345678")) ++
                  [ (at 2 1 1, nOff)
                  , (at 3 1 1, nOn)
                  , (at 3 2 1, nOff)
                  , (at 3 2 2, cC 20 127)
                  , (at 3 2 16, nOn)
                  , (at 4 1 1, nOff)
                  ]

main :: IO ()
main = putStrLn "Hello, world!" >> jebediahMain cfg eL ignoreIncoming
