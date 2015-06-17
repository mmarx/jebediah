module Main (main) where

import Control.Applicative ((<$>))
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
channels = [ (0, Electro4)
           , (1, Electro4)
             -- , (2, Blofeld)
             -- , (3, BassStationII)
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
showMsg (M.Channel c) = putStr ("[" <> show chan <> "] ") >>
                          (showChan instr $ C.messageBody c)
  where chan = C.fromChannel $ C.messageChannel c
        instr = instrumentFromChannel channels chan
showMsg _ = return ()

showChan :: Control i => i -> C.Body -> IO ()
showChan instr (C.Voice v) = showVoice instr v
showChan _ _ = return ()

showVoice :: Control i => i -> V.T -> IO ()
showVoice instr (V.Control cc cv) =  putStrLn $ showCC instr cc cv
showVoice _ _ = return ()

main :: IO ()
main = jebediahMain cfg empty showMsgs
