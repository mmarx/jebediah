{-# LANGUAGE TupleSections #-}
module Main (main) where

import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Data.Default (def)
import Data.EventList.Absolute.TimeBody ( duration
                                        , getTimes
                                        , mapTime
                                        )
import Data.Function (on)
import Data.List (sortBy)
import Data.Ratio ((%))

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
cfg = def { beatsPerMinute = 30
          , beatsPerMeasure = 2
          , targetPorts = targets
          }

at :: Int -> Int -> Int -> EventTime
at m b s = J.at cfg (Measure m) (Beat b) (Subdivision s)

dur :: MIDIEventList -> Int
dur = unEventTime . duration

data Rhythm = Note Int | Rest Int

eighth :: Int
eighth = floor $ (subdivisions cfg) % (2 * beatsPerMeasure cfg)

sixteenth :: Int
sixteenth = floor $ eighth % 2

dottedEighth :: Int
dottedEighth = floor $ eighth * 3 % 2

ayyub :: [Rhythm]
ayyub = [ Note dottedEighth
        , Note sixteenth
        , Note eighth
        , Note eighth
        ]

ayyub' :: [Rhythm]
ayyub' = [ Note eighth
         , Rest sixteenth
         , Note sixteenth
         , Note eighth
         , Note eighth
         ]

malfuf :: [Rhythm]
malfuf = [ Note dottedEighth
         , Note sixteenth
         , Rest eighth
         , Note eighth
         ]

malfuf' :: [Rhythm]
malfuf' = [ Note eighth
          , Rest sixteenth
          , Note sixteenth
          , Rest eighth
          , Note eighth
          ]

hijazKar :: [Int]
hijazKar = [0, 1, 4, 5, 7, 9, 10, 12]

hijaz :: [Int]
hijaz = [0, 1, 4, 5, 7, 8, 10, 12]

delay :: Int -> MIDIEventList -> MIDIEventList
delay offset = mapTime $ EventTime . (offset+) . unEventTime

melody :: Int -> [Int] -> [Rhythm] -> [Int] -> MIDIEventList
melody chan notes rhythm velos = fromPairList $
                                 (EventTime *** toChannel chan) <$>
                                 (sortBy (compare `on` fst) $
                                  go 0 0 [] rhythm notes velos)
  where go :: Int -> Int -> [(Int, C.Body)] -> [Rhythm] -> [Int] -> [Int] -> [(Int, C.Body)]
        go _ _ _ [] _ _ = error "No rhythm"
        go _ _ _ _ _ [] = error "No velocities"
        go _ _ acc _ [] _ = acc
        go bar off acc ((Rest len):rhs) ns vs
          | off + len >= subdivisions cfg = go (bar + 1) (subdivisions cfg - off - len) acc rhs ns vs
          | otherwise = go bar (off + len) acc rhs ns vs
        go bar off acc ar@((Note len):rhs) an@(note:ns) av@(vel:vs)
          | off >= subdivisions cfg = go (bar + 1) (subdivisions cfg - off) acc ar an av
          | off + len >= subdivisions cfg = go (bar + 1) (subdivisions cfg - off - len)
                                            ((pos, noteOn note vel):(pos + len, noteOff note 0):acc)
                                            rhs ns vs
          | otherwise = go bar (off + len)
                        ((pos, noteOn note vel):(pos + len, noteOff note 0):acc)
                        rhs ns vs
          where pos = bar * subdivisions cfg + off

phrase :: Int -> [Int] -> [Int] -> [Int]
phrase base scale steps = concat . zipWith go steps $ tail steps
  where go from to = (base+) . toScale <$> step from to
        step from to
          | from <= to = enumFromTo from to
          | otherwise = enumFromThenTo from (from - 1) to
        toScale idx
          | idx < 0 = - toScale (-idx)
          | otherwise = scale !! idx

drone :: Int -> Int -> Int -> Int -> MIDIEventList
drone chan len note vel = fromPairList $ (EventTime *** toChannel chan) <$>
                          [ (0, noteOn note vel)
                          , (len, noteOff note 0)
                          ]

eL :: MIDIEventList
eL = foldl1 (mergeBy (\ _ _ -> True))
     [ line
     , drone 3 (dur line) 26 64
     ]
  where mel = [0, 6, -3, 1, -5, 3, 3, -1, 2, -6, -2, -5, 0, 0, 0, -2, 0, 0, 0, 3, 3, 0, 0]
        line = melody 2 (phrase 62 hijaz (mel ++ reverse mel)) (cycle malfuf') (cycle $ [32..96] ++ [96, 95 .. 32])


main :: IO ()
main = jebediahMain cfg eL ignoreIncoming
