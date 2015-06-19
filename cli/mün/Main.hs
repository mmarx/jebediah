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
                       ]

cfg :: Config
cfg = def { beatsPerMinute = 120
          , beatsPerMeasure = 2
          , targetPorts = targets
          }

at :: Int -> Int -> Int -> EventTime
at m b s = J.at cfg (Measure m) (Beat b) (Subdivision s)

eL :: MIDIEventList
eL = fromPairList []

main :: IO ()
main = jebediahMain cfg eL ignoreIncoming
