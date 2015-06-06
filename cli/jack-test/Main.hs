module Main (main) where

import Control.Applicative ((<$>))
import Data.Default (def)

import qualified Sound.MIDI.Controller as MC
import qualified Sound.MIDI.Message as M
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V

import Jebediah.JACK hiding (at)
import qualified Jebediah.JACK as J

targets :: [PortName]
targets = PortName <$> [ "Bass-Station-II:midi/capture_1"
                       , "Nord-Electro-4:midi/capture_1"
                       , "MIDISPORT-2x2-Anniv:midi/capture_1"
                       , "midi-monitor:input"
                       ]

nOn = M.Channel $ C.Cons { C.messageChannel = C.toChannel 1
                         , C.messageBody = C.Voice $ V.NoteOn (V.toPitch 60) (V.toVelocity 60)
                         }
nOff = M.Channel $ C.Cons { C.messageChannel = C.toChannel 1
                          , C.messageBody = C.Voice $ V.NoteOff (V.toPitch 60) (V.toVelocity 60)
                          }
cC c v = M.Channel $ C.Cons { C.messageChannel = C.toChannel 1
                        , C.messageBody = C.Voice $ V.Control (MC.fromInt c) v
                        }

cfg :: Config
cfg = def { beatsPerMinute = 120
          , beatsPerMeasure = 2
          , targetPorts = targets
          }

at :: Int -> Int -> Int -> EventTime
at m b s = J.at cfg (Measure m) (Beat b) (Subdivision s)

eL :: MIDIEventList
eL = fromPairList [ (at 1 1 1, cC 20 0)
                  , (at 1 1 1, nOn)
                  , (at 2 1 1, nOff)
                  , (at 3 1 1, nOn)
                  , (at 3 2 1, nOff)
                  , (at 3 2 2, cC 20 127)
                  , (at 3 2 16, nOn)
                  , (at 4 1 1, nOff)
                  ]

main :: IO ()
main = putStrLn "Hello, world!" >> jebediahMain cfg eL
