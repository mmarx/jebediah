module Jebediah.MIDI.Messages
    where

import Data.Maybe (fromMaybe)

import qualified Sound.MIDI.Message as Message
import qualified Sound.MIDI.Message.Channel as Channel
import qualified Sound.MIDI.Message.Channel.Voice as Voice

import Jebediah.MIDI.Instrument
import Jebediah.MIDI.Nord.Lead2X (lead2X)
import Jebediah.MIDI.Nord.Electro4 (electro4)
import Jebediah.MIDI.Novation.BassStationII (bassStationII)
import Jebediah.MIDI.Waldorf.Blofeld (blofeld)

toChannel :: Int -> Channel.Body -> Message.T
toChannel c body = Message.Channel Channel.Cons
                   { Channel.messageChannel = Channel.toChannel c
                   , Channel.messageBody = body
                   }

noteOn :: Int -> Int -> Channel.Body
noteOn = pitchVelocity Voice.NoteOn

noteOff :: Int -> Int -> Channel.Body
noteOff = pitchVelocity Voice.NoteOff

pitchVelocity :: (Voice.Pitch -> Voice.Velocity -> Voice.T) -> Int -> Int -> Channel.Body
pitchVelocity evt p v = Channel.Voice $ evt
                        (Voice.toPitch p)
                        (Voice.toVelocity v)

nrpn :: Int -> Int -> Int -> [Channel.Body]
nrpn msb lsb value = [ controlChange 99 msb
                     , controlChange 98 lsb
                     , controlChange 6 value
                     ]

data Instrument = UnknownInstrument | Electro4 | Lead2X | BassStationII | Blofeld
type ChannelMap = [(Int, Instrument)]

instance Control (Instrument) where
  controlNames UnknownInstrument = []
  controlNames Electro4 = controlNames electro4
  controlNames Lead2X = controlNames lead2X
  controlNames BassStationII = controlNames bassStationII
  controlNames Blofeld = controlNames blofeld

instrumentFromChannel :: ChannelMap -> Int -> Instrument
instrumentFromChannel cs ch = fromMaybe UnknownInstrument $ lookup ch cs
