module Jebediah.Random
       ( newStdGen
       , randomDegree
       , randomMelody
       ) where

import Control.Applicative ((<$>))
import Data.Foldable (toList)
import Data.Sequence (unfoldl)
import System.Random ( RandomGen
                     , newStdGen
                     , randomR
                     )

randomDegree :: RandomGen g => g -> (Int, g)
randomDegree = randomR (-6, 6)

randomNext :: RandomGen g => g -> Int -> (Int, g)
randomNext g prev = go $ randomR (0, 24) g
  where go :: RandomGen g => (Int, g) -> (Int, g)
        go (r, g') = (clamp prev $ go' r, g')
        go' r
          | r == 0 = 0
          | 0 < r && r <= 4 = 1
          | 4 < r && r <= 8 = -1
          | 8 < r && r <= 11 = 2
          | 11 < r && r <= 14 = -2
          | 14 < r && r <= 15 = 3
          | 15 < r && r <= 16 = -3
          | 16 < r && r <= 18 = 4
          | 18 < r && r <= 20 = -4
          | 20 < r && r <= 21 = 5
          | 21 < r && r <= 22 = -5
          | 22 < r && r <= 23 = 6
          | otherwise = -6
        clamp p n
          | -6 <= p + n && p + n <= 6 = p + n
          | otherwise = p - n

randomMelody :: RandomGen g => Int -> g -> ([Int], g)
randomMelody len g = let go (0, _) = Nothing
                         go (cnt, (prev, g')) =
                           let next = randomNext g' prev
                            in Just ((cnt - 1, next), next)
                         mel = toList $ unfoldl go
                               (len, randomDegree g)
                     in (fst <$> mel, snd . last $ mel)
