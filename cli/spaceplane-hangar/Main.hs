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

chordify :: [Note] -> Line
chordify ns = Music.line [ Music.chord $
                           [ toNote . transpose i $ n
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

offbeat :: Line -> Line
offbeat l = Music.line . concat $ [splt, mid, splt]
  where beats = split qn l
        mid = reverse . tail . reverse $ beats
        splt = [Music.take en $ last beats]

offbeat' i = Music.delay i . Music.drop i . offbeat

sub :: [Note]
sub = [ (n, 0, qn)
      | n <- [ F, G, G
            , F, D, C
            , D, F, F
            ]
      ]

c1' = head $ counter sub
c2' = head $ filter (admissible [sub]) $ counter c1'
c3' = head $ filter (admissible [sub, c1']) $ counter c2'

fifth' = map (transpose fifth)
fourth' = map (transpose (-fourth))
fourth'' = map (transpose fourth)

c1 = outline c1'
c2 = outline c2'
c3 = outline c3'

dsub = fifth' sub
ssub = fourth' sub

dc1 = fifth' c1
dc2 = fifth' c2
dc3 = fifth' c3

sc1 = fourth' c1
sc2 = fourth' c2
sc3 = fourth' c3

toLead i t = fromStdMelody i . t . toLine
toLead1 t = toLead Violin t
toLead2 t = toLead Viola t . down 1
toLead3 t = toLead Cello t . down 2
toLead4 t = toLead Contrabass t . up 1

pause = rest $ Music.dur lead1A

base = fromStdMelody Pad2Warm . leg . chordify $ sub ++ c1' ++ dsub ++ c3' ++ ssub
lead1 = lead1A +:+ lead1B +:+ lead1C +:+ lead1D +:+ lead1E
lead2 = lead2A +:+ lead2B +:+ lead2C +:+ lead2D +:+ lead2E
lead3 = lead3A +:+ lead3B +:+ lead3C +:+ lead3D +:+ lead3E
lead4 = lead4A +:+ lead4B +:+ lead4C +:+ lead4D +:+ lead4E

lead1A = toLead1 id $ sub
lead1B = toLead1 offbeat $ c1
lead1C = toLead1 rhythmicize $ sc2
lead1D = toLead1 offbeat $ dc3
lead1E = toLead1 id $ sub

lead2A = pause
lead2B = toLead2 id $ sub
lead2C = toLead2 offbeat $ sc1
lead2D = toLead2 id $ dc2
lead2E = toLead2 offbeat $ c3

lead3A = pause
lead3B = pause
lead3C = toLead3 id $ ssub
lead3D = toLead3 rhythmicize $ dc1
lead3E = toLead3 rhythmicize $ c2

lead4A = pause
lead4B = pause
lead4C = pause
lead4D = toLead4 id $ dsub
lead4E = toLead4 (offbeat . rhythmicize) $ c1

tempo = 1 / 2
stacc = Music.staccato $ tempo / 32
delay = Music.delay $ tempo / 8
leg = Music.legato 2

intro1 = toLead SynthBrass1 (offbeat' en . stacc) . up 3 $ sub ++ sc1 ++ dc2
intro2 = toLead SynthBrass2 (delay . offbeat' en . stacc) . up 2 $ sub ++ sc1 ++ dc2

intro = (((fromStdMelody Pad2Warm . leg . chordify $ sub ++ ssub ++ dsub)
        =:= intro1) +:+ rest (tempo / 8)) =:= intro2

song = changeTempo tempo $
       intro +:+ (base =:= lead1 =:= lead2 =:= lead3 =:= lead4)

patch :: ChannelProgramTable Instrument
patch = [ (Contrabass, (toChannel 4, toProgram 70))
        , (Cello, (toChannel 0, toProgram 47))
        , (Viola, (toChannel 1, toProgram 48))
        , (Violin, (toChannel 3, toProgram 49))
        , (Pad2Warm, (toChannel 2, toProgram 44))
        , (SynthBrass1, (toChannel 0, toProgram 45))
        , (SynthBrass2, (toChannel 1, toProgram 45))
        ]

main :: IO ()
main = play "mididings:in_1" patch song
