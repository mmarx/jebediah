module Main (main) where

import Haskore.Basic.Pitch (Class (..))
import Haskore.Basic.Interval
import Haskore.Music ( rest
                     , (=:=)
                     , (+:+)
                     )
import Haskore.Music.GeneralMIDI ( toChannel
                                 , toProgram
                                 , changeTempo
                                 , fromStdMelody
                                 )
import qualified Haskore.Music.GeneralMIDI as MidiMusic
import Haskore.Basic.Duration (wn, hn, qn, en)

import Jebediah.Alsa
import Jebediah.Counterpoint
import Jebediah.Note hiding (down)

fifth' = map (transpose fifth)
fourth' = map (transpose (-fourth))
fourth'' = map (transpose fourth)

r1 = rest wn +:+ rest wn +:+ rest qn
s1 = [(C, 5, qn), (C, 5, qn), (C, 5, qn), (G, 4, qn), (B, 4, qn), (C, 5, en), (B, 4, en), (A, 4, qn), (G, 4, hn)]
ds1 = fourth' s1
ss1 = fourth'' s1

c1' = head $ counter s1
c1 = outline c1'
c2' = head $ filter (admissible [s1]) $ counter c1'
c2 = outline c2'
c3 = head $ filter (admissible [s1, c1']) $ counter c2'

dc1 = fifth' c1
dc2 = fifth' c2
dc3 = fifth' c3
sc1 = fourth'' c1
sc2 = fourth'' c2
sc3 = fourth'' c3

bass = concat $ [ds1] ++ [c1, c2, c3, ds1, c1]
tenor = concat $ [s1, c1] ++ [c2, c3, s1, dc1, c2] ++ [s1, sc3]
alto = concat $ [ds1, c1, c2] ++ [c3, s1, c1, dc2, c3] ++ [c1, ss1]
soprano = concat $ [s1, c1, c2, c3] ++ [s1, c3, c2, dc1, s1]

bass'' = concat $ [dc2, s1]
soprano'' = concat $ [ds1, c1]

patch :: ChannelProgramTable Instrument
patch = [ (Contrabass, (toChannel 0, toProgram 30))
        , (Cello, (toChannel 1, toProgram 31))
        , (Viola, (toChannel 2, toProgram 32))
        , (Violin, (toChannel 3, toProgram 33))
        ]

down n = MidiMusic.transpose (-12 * n) . toLine

bass' = fromStdMelody Contrabass $ r1 +:+ r1 +:+ r1 +:+ down 5 bass +:+ r1 +:+ down 5 bass''
tenor' = fromStdMelody Cello $ r1 +:+ r1 +:+ down 4 tenor
alto' = fromStdMelody Viola $ r1 +:+ down 3 alto
soprano' = fromStdMelody Violin $ down 2 soprano +:+ r1 +:+ down 2 soprano''

song = changeTempo (1 / 3) $ bass' =:=
       tenor' =:=
       alto' =:=
       soprano'

main :: IO ()
main = play "mididings:in_1" patch song
