{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Jebediah.MIDI.Novation.BassStationII
       where

import Jebediah.MIDI.Instrument

import Sound.MIDI.Message.Channel (Body)

data BassStationII = BassStationII

bassStationII :: BassStationII
bassStationII = BassStationII

ctrlNames :: [(Int, String)]
ctrlNames = sorted
            [ (7, "Patch Volume")
            , (70, "Oscillator 1: Range")
            , (71, "Oscillator 1: Modulation Envelope Depth")
            , (99, "NRPN: Most Significant Byte")
            , (98, "NRPN: Least Significant Byte")
            , (6, "NRPN: Value")
            ]

instance Control BassStationII where
  controlNames _ = ctrlNames

en :: Enum cv => String -> cv -> [Body]
en = enumerable bassStationII
