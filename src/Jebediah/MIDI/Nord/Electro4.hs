module Jebediah.MIDI.Nord.Electro4
    where

import Control.Arrow ((***))
-- import Jebediah.JACK ( MIDIEventList
--                      , fromPairList
--                      , merge
--                      )
import Jebediah.MIDI.Messages

import qualified Sound.MIDI.Message.Channel as Channel

data Manual = Lower | Upper

drawbars :: Manual -> String -> [Channel.Body]
drawbars m = map (uncurry controlChange . ((cc+) *** drawbar)) . zip [0..8]
    where cc = case m of
                 Upper -> 16
                 Lower -> 70
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
