{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Jebediah.MIDI.Nord.Electro4
    where

import Control.Arrow ((***))
import Data.Function (on)
import Data.List (sortBy)
import Jebediah.MIDI.Instrument

import Sound.MIDI.Message.Channel (Body)

mkEnum "Manual" [enum|
                 Lower 0
                 Upper 127|]

mkEnum "OrganModel" [enum|
                     B3 0
                     Farfisa 64
                     Vox 127|]

mkEnum "RotarySpeed" [enum|
                      Slow 0
                      Fast 127|]

mkEnum "RotaryStop" [enum|
                     Running 0
                     Stopped 127|]


mkEnum "VibMode" [enum|
                  V1 0
                  C1 26
                  V2 51
                  C2 77
                  V3 102
                  C3 127|]

mkEnum "Instrument" [enum|
                     Organ 0
                     Piano 127|]
mkEnum "PianoType" [enum|
                    Grand 0
                    Upright 26
                    Tines 51
                    Reeds 77
                    Clavinets 102
                    Samples 127|]

mkEnum "Toggle" [enum|
                 Off 0
                 On 127|]

data Electro4 = Electro4

electro4 :: Electro4
electro4 = Electro4

ctrlNames :: [(Int, String)]
ctrlNames = sortBy (compare `on` fst)
            [ (16, "Upper: Drawbar 1")
            , (17, "Upper: Drawbar 2")
            , (18, "Upper: Drawbar 3")
            , (19, "Upper: Drawbar 4")
            , (20, "Upper: Drawbar 5")
            , (21, "Upper: Drawbar 6")
            , (22, "Upper: Drawbar 7")
            , (23, "Upper: Drawbar 8")
            , (24, "Upper: Drawbar 9")
            , (70, "Lower: Drawbar 1")
            , (71, "Lower: Drawbar 2")
            , (72, "Lower: Drawbar 3")
            , (73, "Lower: Drawbar 4")
            , (74, "Lower: Drawbar 5")
            , (75, "Lower: Drawbar 6")
            , (76, "Lower: Drawbar 7")
            , (77, "Lower: Drawbar 8")
            , (78, "Lower: Drawbar 9")
            , (9, "Organ Model")
            , (3, "Preset/Split")
            , (88, "Perc Decay")
            , (89, "Perc Level")
            , (64, "Sustain Pedal")
            , (11, "Control Pedal")
            , (82, "Rotary Speed")
            , (79, "Rotary Stop Mode")
            , (84, "Vibrato Mode")
            , (85, "Upper: Vibrato")
            , (107, "Lower: Vibrato")
            , (87, "Perc")
            , (95, "Perc Third")
            , (33, "Sample Attack")
            , (34, "Sample Release")
            , (103, "Clavinet: EQ Brilliance")
            , (104, "Clavinet: EQ Treble")
            , (105, "Clavinet: EQ Medium")
            , (106, "Clavinet: EQ Soft")
            , (13, "Instrument Selection")
            , (45, "Clavinet: Selection")
            , (12, "Piano Type")
            , (44, "Piano Model")
            , (83, "Piano Mono")
            , (99, "Piano Dynamics")
            , (98, "Accoustics")
            , (27, "Upper: Octave Shift")
            , (28, "Lower: Octave Shift")
            , (29, "Piano: Octave Shift")
            , (115, "EQ")
            , (113, "EQ Treble")
            , (116, "EQ Mid")
            , (117, "EQ Mid Frequency")
            , (118, "EQ Bass")
            , (7, "Gain")
            , (63, "Effect 1: Rate")
            , (60, "Effect 1: Selection")
            , (69, "Effect 1")
            , (62, "Effect 2: Rate")
            , (61, "Effect 2: Selection")
            , (80, "Effect 2")
            , (111, "Speaker: Drive")
            , (81, "Speaker: Selection")
            , (86, "Speaker")
            , (102, "Reverb: Wet/Dry")
            , (96, "Reverb: Selection")
            , (97, "Reverb")
            , (92, "Delay: Rate")
            , (93, "Delay: Ping Pong")
            , (94, "Delay")
            ]

instance Control Electro4 where
    controlNames _ = ctrlNames

drawbars :: Manual -> String -> [Body]
drawbars m = map (uncurry controlChange . ((cc+) *** drawbar)) . zip [0..8]
    where cc = controlId electro4 $ case m of
                 Upper -> "Upper: Drawbar 1"
                 Lower -> "Lower: Drawbar 1"
          drawbar '0' = 0
          drawbar '1' = 16
          drawbar '2' = 32
          drawbar '3' = 48
          drawbar '4' = 64
          drawbar '5' = 80
          drawbar '6' = 96
          drawbar '7' = 112
          drawbar '8' = 127
          drawbar _ = 0

en :: Enum cv => String -> cv -> [Body]
en = enumerable electro4

manual :: Manual -> [Body]
manual = en "Preset/Split"

organModel :: OrganModel -> [Body]
organModel = en "Organ Model"

rotarySpeed :: RotarySpeed -> [Body]
rotarySpeed = en "Rotary Speed"

rotaryStopMode :: RotaryStop -> [Body]
rotaryStopMode = en "Rotary Stop Mode"

vibratoMode :: VibMode -> [Body]
vibratoMode = en "Vibrato Mode"

vibrato :: Manual -> Toggle -> [Body]
vibrato Lower = en "Lower: Vibrato"
vibrato Upper = en "Upper: Vibrato"

instrument :: Instrument -> [Body]
instrument = en "Instrument Selection"

pianoType :: PianoType -> [Body]
pianoType = en "Piano Type"
