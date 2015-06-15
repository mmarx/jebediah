module Jebediah.MIDI.Messages
    where

import Control.Applicative ((<$>))
import Data.Maybe ( fromJust
                  , fromMaybe
                  )
import Data.Monoid ((<>))
import Data.Tuple (swap)

import qualified Sound.MIDI.Message as Message
import qualified Sound.MIDI.Controller as Controller
import qualified Sound.MIDI.Message.Channel as Channel
import qualified Sound.MIDI.Message.Channel.Voice as Voice

toChannel :: Int -> Channel.Body -> Message.T
toChannel c body = Message.Channel Channel.Cons
                   { Channel.messageChannel = Channel.toChannel c
                   , Channel.messageBody = body
                   }

noteOn :: Int -> Int -> Channel.Body
noteOn = pitchVelocity Voice.NoteOn

noteOff :: Int -> Int -> Channel.Body
noteOff = pitchVelocity Voice.NoteOff

controlChange :: Int -> Int -> Channel.Body
controlChange c v = Channel.Voice $ Voice.Control (Controller.fromInt c) v

pitchVelocity :: (Voice.Pitch -> Voice.Velocity -> Voice.T) -> Int -> Int -> Channel.Body
pitchVelocity evt p v = Channel.Voice $ evt
                        (Voice.toPitch p)
                        (Voice.toVelocity v)

class Control a where
    controlNames :: a -> [(Int, String)]
    controlIds :: a -> [(String, Int)]
    controlIds a = swap <$> controlNames a
    controlName :: a -> Int -> String
    controlName a cc = fromMaybe "unknown controller" $ lookup cc $ controlNames a
    controlId :: a -> String -> Int
    controlId a cc = fromJust $ lookup cc $ controlIds a
    showCC :: a -> Controller.T -> Controller.Value -> String
    showCC a cc cv = cn <> " (" <> show (Controller.toInt cc) <> "): " <> show cv
        where cn = controlName a $ Controller.toInt cc
