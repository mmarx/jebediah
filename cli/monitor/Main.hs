module Main (main) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Default (def)
import Data.EventList.Absolute.TimeBody (empty)
import Data.Monoid ((<>))

import qualified Sound.MIDI.Controller as MC
import qualified Sound.MIDI.Message as M
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V

import Jebediah.JACK
import Jebediah.MIDI.Messages
import Jebediah.MIDI.Instrument

sources :: [PortName]
sources = PortName <$> [ "Bass-Station-II:midi/playback_1"
                       , "Nord-Electro-4:midi/playback_1"
                       , "MIDISPORT-2x2-Anniv:midi/playback_1"
                       , "Blofeld:midi/playback_1"
                       ]

channels :: ChannelMap
channels = [ (0, Blofeld)
           , (1, Electro4)
           , (2, Electro4)
           , (3, BassStationII)
           , (4, Lead2X)
           , (5, Lead2X)
           , (6, Lead2X)
           , (7, Lead2X)
           ]

cfg :: Config
cfg = def { sourcePorts = sources }

showMsgs :: [M.T] -> IO ()
showMsgs = mapM_ showMsg

showMsg :: M.T -> IO ()
showMsg msg = do
  let str = showMsg' msg
  when (not . null $ str) $ putStrLn str
  where
    showMsg' (M.Channel c) = showChan (C.fromChannel $ C.messageChannel c) $ C.messageBody c
    showMsg' _ = ""

showChan :: Int -> C.Body -> String
showChan chan body = let
  prefix = "[" <> show chan <> "] "
  instr = instrumentFromChannel channels chan
  suffix = showChan' body
  showChan' (C.Voice v) = showVoice instr v
  showChan' _ = ""
  in if null suffix then "" else prefix <> suffix

showVoice :: Control i => i -> V.T -> String
showVoice instr (V.Control cc cv) = showCC instr cc cv
showVoice _ _ = ""

main :: IO ()
main = jebediahMain cfg empty showMsgs
