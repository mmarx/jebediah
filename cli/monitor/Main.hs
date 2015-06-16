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
import Jebediah.MIDI.Nord.Electro4

sources :: [PortName]
sources = PortName <$> [ "Bass-Station-II:midi/playback_1"
                       , "Nord-Electro-4:midi/playback_1"
                       , "MIDISPORT-2x2-Anniv:midi/playback_1"
                       ]

cfg :: Config
cfg = def { sourcePorts = sources }

showMsgs :: [M.T] -> IO ()
showMsgs = mapM_ showMsg

showMsg :: M.T -> IO ()
showMsg (M.Channel c) = putStr ("[" <> show chan <> "] ") >>
                          (showChan $ C.messageBody c)
  where chan = C.fromChannel $ C.messageChannel c
showMsg _ = return ()

showChan :: C.Body -> IO ()
showChan (C.Voice v) = showVoice v
showChan _ = return ()

showVoice :: V.T -> IO ()
showVoice (V.Control cc cv) =  putStrLn $ showCC electro4 cc cv
showVoice _ = return ()

main :: IO ()
main = jebediahMain cfg empty showMsgs
