{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Jebediah.MIDI.Waldorf.Blofeld
       where

import Jebediah.MIDI.Instrument

import Sound.MIDI.Message.Channel (Body)

data Blofeld = Blofeld

blofeld :: Blofeld
blofeld = Blofeld

ctrlNames :: [(Int, String)]
ctrlNames = sorted
            [ (27, "Oscillator 1: Range")
            ]

instance Control Blofeld where
  controlNames _ = ctrlNames

en :: Enum cv => String -> cv -> [Body]
en = enumerable blofeld
