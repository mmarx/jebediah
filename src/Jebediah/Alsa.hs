module Jebediah.Alsa ( ChannelProgramTable
                     , Instrument (..)
                     , play
                     )
       where

import Control.Monad (void)
import System.Process (rawSystem)

import Haskore.Interface.MIDI.InstrumentMap (ChannelProgramTable)
import Haskore.Interface.MIDI.Write (fromRhythmicPerformance)
import Haskore.Music.GeneralMIDI ( Instrument (..)
                                 , T
                                 )
import Haskore.Performance.Fancy (floatFromMusic)
import Sound.MIDI.File.Save (toFile)

play :: String -> ChannelProgramTable Instrument -> T -> IO ()
play dstPort patch song = do
  let name = "out.mid"
  toFile name $ fromRhythmicPerformance [] patch $ floatFromMusic song
  void $ rawSystem "aplaymidi" ["-p", dstPort, name]
