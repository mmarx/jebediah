{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Jebediah.MIDI.Nord.Lead2X
    where

import Data.Function (on)
import Data.List (sortBy)
import Jebediah.MIDI.Instrument

import Sound.MIDI.Message.Channel (Body)

data Lead2X = Lead2X

lead2X :: Lead2X
lead2X = Lead2X

ctrlNames :: [(Int, String)]
ctrlNames = sortBy (compare `on` fst)
            [ (7, "Gain")
            , (17, "Octave Shift")
            , (18, "Modulation Wheel Destination")
            , (16, "Unison")
            , (15, "Polyphony")
            , (65, "Portamento")
            , (5, "Portamento Time")
            , (19, "LFO 1: Rate")
            , (20, "LFO 1: Waveform")
            , (21, "LFO 1: Destination")
            , (22, "LFO 1: Amount")
            , (23, "LFO 2: Rate")
            , (24, "LFO 2: Destination")
            , (25, "LFO 2: Amount")
            , (26, "Modulation Envelope: Attack")
            , (27, "Modulation Envelope: Decay")
            , (28, "Modulation Envelope: Destination")
            , (29, "Modulation Envelope: Amount")
            , (30, "Oscillator 1: Waveform")
            , (31, "Oscillator 2: Waveform")
            , (78, "Oscillator 2: Semitones")
            , (33, "Oscillator 2: Fine Tune")
            , (70, "Oscillator FM Depth")
            , (34, "Oscillator 2: Keyboard Tracking")
            , (79, "Oscillator Pulse Width")
            , (35, "Oscillator Sync")
            , (8, "Oscillator Balance")
            , (73, "Amplifier Envelope: Attack")
            , (36, "Amplifier Envelope: Decay")
            , (37, "Amplifier Envelope: Sustain")
            , (72, "Amplifier Envelope: Release")
            , (38, "Filter Envelope: Attack")
            , (39, "Filter Envelope: Decay")
            , (40, "Filter Envelope: Sustain")
            , (41, "Filter Envelope: Release")
            , (44, "Filter Mode")
            , (74, "Filter Cutoff")
            , (42, "Filter Resonance")
            , (43, "Filter Envelope Amount")
            , (45, "Filter Velocity")
            , (46, "Filter Keyboard Track")
            , (80, "Filter Distortion")
            , (32, "Bank Select")
            ]

instance Control Lead2X where
    controlNames _ = ctrlNames

en :: Enum cv => String -> cv -> [Body]
en = enumerable lead2X
