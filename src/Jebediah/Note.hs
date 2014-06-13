module Jebediah.Note ( Line
                     , Note
                     , Movement ( Up
                                , Down
                                , None
                                )
                     , Dur
                     , clamp
                     , duration
                     , interval
                     , octave
                     , pitch
                     , relative
                     , toLine
                     , toNote
                     , transpose
                     )
       where

import Prelude hiding ( fst
                      , snd
                      )

import Haskore.Basic.Pitch ( Class ()
                           , Octave
                           )
import qualified Haskore.Basic.Pitch as Pitch
import Haskore.Melody (note')
import qualified Haskore.Melody as Melody
import Haskore.Melody.Standard ( NoteAttributes
                               , na
                               )
import Haskore.Music (line)
import qualified Haskore.Music as Music
import Medium.Temporal (Dur)

type Note = (Class, Octave, Dur)
data Movement = Up | Down | None
              deriving (Read, Show, Eq)

pitch :: Note -> Int
pitch (_, cls, _) = cls

octave :: Note -> Class
octave (oct, _, _) = oct

duration :: Note -> Dur
duration (_, _, dur) = dur

relative :: Note -> Int
relative (cls, oct, _) = oct * 12 + Pitch.classToInt cls

interval :: Note -> Note -> Int
interval fst snd = relative fst - relative snd

transpose :: Int -> Note -> Note
transpose i (cls, oct, dur) = (cls', oct', dur)
  where (oct', cls') = Pitch.transpose i (oct, cls)

clamp :: Note -> Note -> Note
clamp (cls, oct, _) to = (cls, oct, duration to)

toNote :: Note -> Melody.T NoteAttributes
toNote (cls, oct, dur) = note' cls oct dur na

type Line = Music.T (Melody.Note NoteAttributes)

toLine :: [Note] -> Line
toLine = line . map toNote
