module Main (main) where

import Control.Applicative ((<$>))

import Haskore.Basic.Duration
import Haskore.Basic.Interval
import Haskore.Basic.Pitch (Class (..))
import Haskore.Music ( rest
                     , (=:=)
                     , (+:+)
                     )
import qualified Haskore.Music as Music
import Haskore.Melody (noteToPitch)
import Haskore.Music.GeneralMIDI ( changeTempo
                                 , fromStdMelody
                                 , toChannel
                                 , toProgram
                                 )

import Jebediah.Alsa
import Jebediah.Counterpoint
import Jebediah.Note

s1 :: [Note]
s1 = [ (n, 0, qn)
     | n <- [ F, G, G
           , F, D, C
           , D, F, F
           ]
     ]

s2 :: [Note]
s2 = [ (A, 0, hn)
     , (G, 0, qn)
     , (A, 0, qn)
     , (C, 1, qn)
     , (D, 1, qn)
     , (A, 0, hn)
     ]

s2' = widen (9/8) s2

fractalise :: [Note] -> [Note]
fractalise r = concat [r', reverse r'', r'', r']
  where r' = narrow 3 r
        r'' = narrow 6 r

split :: Dur -> Line -> [Line]
split d l = go (Music.dur l) l []
  where go 0 _ ls = ls
        go _ l' ls = let h = Music.take d l'
                         r = Music.drop d l'
                         d' = Music.dur r
                     in go d' r $ h:ls

rhythmicize :: Line -> Line
rhythmicize = Music.line . zipWith go (cycle [1..9] :: [Int]) . split qn
  where go 1 = Music.take den
        go 2 = Music.delay sn
        go 4 = Music.take den
        go 6 = Music.delay sn
        go 7 = Music.delay sn
        go 9 = Music.take den
        go _ = id

offbeat :: Line -> Line
offbeat l = Music.line . concat $ [splt, mid, splt]
  where beats = split qn l
        mid = reverse . tail . reverse $ beats
        splt = [Music.take en $ last beats]

theScale :: [Class]
theScale = [C, D, F, G, A]

chordify :: [Note] -> Line
chordify ns = Music.line [ Music.chord $
                           toNote <$>
                           filter ((`elem` theScale) . (\(cls, _, _) -> cls))
                           [ transpose i $ n
                           | i <- [ 0
                                 , 4
                                 , 7
                                 , 12
                                 , 24
                                 , -9
                                 , -12
                                 , -24
                                 ]
                           ]
                         | n <- ns
                         ]

toLead i t = fromStdMelody i . t . toLine . fractalise
toLead1 t = toLead Violin t
toLead2 t = toLead Viola t . down 1
toLead3 t = toLead Cello t . down 2
toLead4 t = toLead TremoloStrings t . up 1

pause = rest $ Music.dur lead1A
leg = Music.legato 2

base = fromStdMelody Pad2Warm . leg . chordify $ s1 ++ s2' ++ s1 ++ s2' ++ s1
lead1 = lead1A +:+ lead1B +:+ lead1C +:+ lead1D +:+ lead1E
lead2 = lead2A +:+ lead2B +:+ lead2C +:+ lead2D +:+ lead2E
lead3 = lead3A +:+ lead3B +:+ lead3C +:+ lead3D +:+ lead3E
lead4 = lead4A +:+ lead4B +:+ lead4C +:+ lead4D +:+ lead4E

lead1A = toLead1 id $ s1
lead1B = toLead1 offbeat . fractalise $ s2'
lead1C = toLead1 rhythmicize . fractalise . fractalise $ s2'
lead1D = toLead1 offbeat . fractalise . fractalise $ s1
lead1E = toLead1 id . fractalise $ s1

lead2A = pause
lead2B = pause
lead2C = toLead2 offbeat . fractalise $ s1
lead2D = toLead2 id . fractalise . fractalise $ s1
lead2E = toLead2 offbeat . fractalise $ s1

lead3A = pause
lead3B = pause
lead3C = pause
lead3D = toLead3 rhythmicize . fractalise $ s1
lead3E = toLead3 rhythmicize . fractalise $ s1

lead4A = pause
lead4B = pause
lead4C = pause
lead4D = pause
lead4E = toLead4 (offbeat . rhythmicize) . fractalise $ s1

song = changeTempo (1 / 8) $
       base =:= lead1 =:= lead2 =:= lead3 =:= lead4

patch :: ChannelProgramTable Instrument
patch = [ (Contrabass, (toChannel 0, toProgram 51))
        , (Cello, (toChannel 1, toProgram 52))
        , (Viola, (toChannel 2, toProgram 53))
        , (Violin, (toChannel 3, toProgram 54))
        , (TremoloStrings, (toChannel 4, toProgram 71))
        , (Pad2Warm, (toChannel 0, toProgram 50))
        ]

main :: IO ()
main = play "mididings:in_1" patch song
