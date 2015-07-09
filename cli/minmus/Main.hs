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
import Jebediah.Random ( newStdGen
                       , randomMelody
                       )

targets :: [PortName]
targets = PortName <$> [ "Bass-Station-II:midi/capture_1"
                       , "Nord-Electro-4:midi/capture_1"
                       , "MIDISPORT-2x2-Anniv:midi/capture_1"
                       , "Blofeld:midi/capture_1"
                       ]

cfg :: Config
cfg = def { beatsPerMinute = 30
          , beatsPerMeasure = 4
          , targetPorts = targets
          }

at :: Int -> Int -> Int -> EventTime
at m b s = J.at cfg (Measure m) (Beat b) (Subdivision s)

bars :: Int -> Int
bars = measure . (1+)

measure :: Int -> Int
measure n = unEventTime $ at n 1 1

dur :: MIDIEventList -> Int
dur = unEventTime . duration

data Rhythm = Note Int | Rest Int

whole :: Int
whole = half * 2

half :: Int
half = quarter * 2

quarter :: Int
quarter = subdivisions cfg

eighth :: Int
eighth = floor $ quarter % 2

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

fours :: [Rhythm]
fours = [ Note quarter
        , Note quarter
        , Note quarter
        , Note quarter
        ]

bells :: [Rhythm]
bells = [ Note half
        , Note half
        , Note half
        , Note half
        ]

hijazKar :: [Int]
hijazKar = [0, 1, 4, 5, 7, 9, 10, 12]

hijaz :: [Int]
hijaz = [0, 1, 4, 5, 7, 8, 10, 12]

major :: [Int]
major = [0, 2, 4, 5, 7, 9, 11, 12]

minor :: [Int]
minor = [0, 2, 3, 5, 7, 8, 10, 12]

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

toScale :: [Int] -> Int -> Int
toScale scale idx
  | idx < 0 = - toScale scale (-idx)
  | otherwise = scale !! idx

phrase :: Int -> [Int] -> [Int] -> [Int]
phrase base scale steps = concat . zipWith go steps $ tail steps
  where go from to = line base scale $ step from to
        step from to
          | from <= to = enumFromTo from to
          | otherwise = enumFromThenTo from (from - 1) to

line :: Int -> [Int] -> [Int] -> [Int]
line base scale steps = (base+) . toScale scale <$> steps

drone :: Int -> Int -> Int -> Int -> MIDIEventList
drone chan len note vel = fromPairList $ (EventTime *** toChannel chan) <$>
                          [ (0, noteOn note vel)
                          , (len, noteOff note 0)
                          ]

pad :: Int -> Int -> Int -> Int -> MIDIEventList
pad = pad' [ -24, -17, -12, -5, 0, 7, 12, 14, 24 ]

pad' :: [Int] -> Int -> Int -> Int -> Int -> MIDIEventList
pad' offs chan len base vel = fromPairList $ (EventTime *** toChannel chan) <$>
                              (sortBy (compare `on` fst) $ concat
                               [ [ (0, noteOn (base + n) vel)
                                 , (len, noteOff (base + n) 0)
                                 ]
                               | n <- offs
                               ])

noise :: Int -> Int -> Int -> Int -> Int -> MIDIEventList
noise pr chan len base vel = merge [ prog chan pr
                                   , delay 1 $ pad' [-12, 0, 12] chan (len - 1) base vel
                                   ]

tin :: Int -> Int -> [Int] -> Int -> [Int] -> [Rhythm] -> MIDIEventList
tin chan len scale base vel rhythm = melody chan me rh ve
  where me = take len $ ((base+) . toScale scale) <$> cycle [-6, -4, 2, 6]
        rh = cycle rhythm
        ve = cycle vel

prog :: Int -> Int -> MIDIEventList
prog chan pr = fromPairList [(EventTime 0, toChannel chan $ programChange pr)]

eL :: IO MIDIEventList
eL = do
  g <- newStdGen
  let base = 57
      blo, eLow, eUp, bs2, ld1, ld2, ld3, ld4 :: Int
      (blo, eLow, eUp, bs2, ld1, ld2, ld3, ld4) = (0, 1, 2, 3, 4, 5, 6, 7)
      (mel, g') = randomMelody 32 g
      (mel', g'') = randomMelody 8 g'
      line1 = melody eLow (line base minor mel) (cycle fours) (cycle [64])
      line2 = melody eUp (line 72 major mel') (cycle fours) (cycle [64])
      ini = merge [ prog blo 1
                  , prog eUp 7
                  , prog bs2 1
                  , prog ld1 1
                  , prog ld2 1
                  , prog ld3 1
                  , prog ld4 1
                  ]
      intro = delay (dur ini) $
              merge [ drone bs2 (bars 2) (base - 36) 64
                    , delay (measure 2) $ drone bs2 (bars 1) (base - 33) 64
                    , delay (measure 2 + 64) $ noise 5 eUp 16 base 64
                    , delay (measure 2 + 128) $ pad blo (bars 1) (base - 24) 64
                    , delay (measure 3) $ drone bs2 (bars 1) (base - 36) 64
                    , delay (measure 3 + 128) $ noise 5 eUp 16 base 64
                    , delay (measure 3 + 192) $ pad eUp 32 base 64
                    ]
      a = merge [ prog eUp 7
                , delay 1 $ line1
                , delay 1 $ tin eUp (dur line1) minor (base - 12) [64, 48, 80, 64] bells
                ]
      c = merge [a]
      b = merge [a]
      acab = delay (dur intro) $ merge [ a
                                       , delay (dur a) c
                                       , delay (dur a + dur c) a
                                       , delay (2 * dur a + dur c) b
                                       ]
      outro = delay (dur acab) $ merge [intro, delay (dur intro) intro, delay (dur intro + dur intro) intro]
  return $ merge
    [ ini
    , intro
    , acab
    , outro
    ]

main :: IO ()
main = eL >>= \el -> jebediahMain cfg el ignoreIncoming