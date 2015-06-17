{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Jebediah.MIDI.Instrument where

import Control.Applicative ((<$>))
import Control.Arrow ( (***)
                     , second
                     )
import Data.Maybe ( fromJust
                  , fromMaybe
                  )
import Data.Monoid ((<>))
import Data.Tuple (swap)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified Sound.MIDI.Controller as Controller
import qualified Sound.MIDI.Message.Channel as Channel
import qualified Sound.MIDI.Message.Channel.Voice as Voice

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

controlChange :: Int -> Int -> Channel.Body
controlChange c v = Channel.Voice $ Voice.Control (Controller.fromInt c) v

enumerable :: (Control i, Enum cc) => i -> String -> cc -> [Channel.Body]
enumerable instr ctrl value = [controlChange cc cv]
    where cc = controlId instr ctrl
          cv = fromEnum value

pairFromList :: [a] -> (a, a)
pairFromList [a, b] = (a, b)
pairFromList _ = error "List is not a pair"

enum :: QuasiQuoter
enum = QuasiQuoter { quoteExp = qe }
  where qe str = do
          let lst = filter (not . null) $ words <$> lines str
              ps :: [(String, Integer)]
              ps = (second read . pairFromList) <$> lst
          [|ps|]

mkEnum :: String -> [(String, Integer)] -> Q [Dec]
mkEnum name' pairs' = do
  decl <- dataD (cxt []) name [] cons [''Eq, ''Bounded, ''Show, ''Read]
  inst <- instanceD (cxt []) (appT (conT ''Enum) (conT name))
          [ funD (mkName "toEnum") (genTo <$> pairs)
          , funD (mkName "fromEnum") (genFrom <$> pairs)
          ]
  return [decl, inst]
  where name = mkName name'
        pairs = (mkName *** integerL) <$> pairs'
        cons = (\n -> normalC n []) . fst <$> pairs
        genTo (c, v) = clause [litP v] (normalB $ conE c) []
        genFrom (c, v) = clause [conP c []] (normalB $ litE v) []
