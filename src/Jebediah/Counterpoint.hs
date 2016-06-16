module Jebediah.Counterpoint
       where

import Prelude hiding ( fst
                      , snd
                      )
import Data.Ratio ((%))

import Haskore.Basic.Duration ( fromRatio
                              , scale
                              )

import Jebediah.Note

movement' :: Int -> Movement
movement' n
  | n > 0 = Up
  | n < 0 = Down
  | otherwise = None

movement :: Note -> Note -> Movement
movement fst snd = movement' $ interval fst snd

counter :: [Note] -> [[Note]]
counter [] = []
counter d@(x:xs) = go Nothing x xs [[]]
  where combine :: Maybe Note -> Note -> Maybe Note -> [[Note]] -> [[Note]]
        combine prev cur next res = filter (admissible [d]) [ c ++ [n]
                                                            | c <- res
                                                            , n <- counterNext ((,) (last c) <$> prev) cur next
                                                            ]
        go :: Maybe Note -> Note -> [Note] -> [[Note]] -> [[Note]]
        go prev cur [] res = combine prev cur Nothing res
        go prev cur (next:rest) res = go (Just cur) next rest (combine prev cur (Just next) res)

nexts :: Note -> [Note]
nexts base = [transpose i base | i <- [-7 .. 7]]

nextsNotDir :: Movement -> Note -> [Note]
nextsNotDir dir base = filter ((dir /=) . movement base) $ nexts base

ladder :: Note -> Int -> [Note]
ladder base@(_, _, dur) steps
  | steps >= 0 = rungs [0 .. steps]
  | otherwise = rungs $ reverse [steps .. 0]
  where rungs sts = clamp' [ transpose step base
                           | step <- sts
                           , ((relative base + step) `mod` 12) `elem` [ 0, 2, 4, 5, 7, 9, 11 ]
                           ]

        clmp dur' (cls, oct, _) = (cls, oct, dur')
        clamp' [] = []
        clamp' l@(n:ns) = let dur' = fromRatio $ 1 % toInteger (length l + 1)
                              hd = clmp (scale (2 * dur') dur) n
                              rst = map (clmp $ scale dur' dur) ns
                          in hd:rst

totalDuration :: [Note] -> Dur
totalDuration = sum . map duration

outline' :: Note -> Note -> [Note]
outline' from to
  | abs dist <= 1 = [from, to]
  | otherwise = ladder from (1 - dist) ++ [to]
  where dist = interval from to

outline :: [Note] -> [Note]
outline [] = []
outline (n:ns) = go n ns []
  where go :: Note -> [Note] -> [Note] -> [Note]
        go from [to] res = res ++ outline' from to
        go from (to:next:rest) res
          | abs (interval from to) <= 1 = go to (next:rest) (res ++ [from])
          | otherwise = go next rest (res ++ outline' from to)
        go n' [] res = res ++ [n']

counterNext :: Maybe (Note, Note) -> Note -> Maybe Note -> [Note]
counterNext Nothing cur Nothing = nexts cur
counterNext Nothing cur (Just _) = nexts cur
counterNext (Just (prev, prev')) cur _ = map (`clamp` cur) $ nextsNotDir (movement prev cur) prev'

consonant :: Note -> Note -> Bool
consonant d c = perfect d c || abs (interval d c) `elem` [3, 4, 8, 9]

perfect :: Note -> Note -> Bool
perfect d c = abs ( interval d c) `elem` [0, 5, 7, 12]

parallel :: (Note, Note) -> (Note, Note) -> Bool
parallel (prev, prev') (d, c) = interval prev d == interval prev' c

admissible :: [[Note]] -> [Note] -> Bool
admissible ctxs ln = all (\ ctx -> admissible' Nothing ctx ln) ctxs

admissible' :: Maybe (Note, Note) -> [Note] -> [Note] -> Bool
admissible' _ [] _ = True
admissible' _ _ [] = True
admissible' Nothing (d:ds) (c:cs) = perfect d c && admissible' (Just (d, c)) ds cs
admissible' (Just _) [d] [c] = perfect d c
admissible' (Just (prev, prev')) (d:ds) (c:cs) = consonant d c
                                                 && not (parallel (prev, prev') (d, c))
                                                 && admissible' (Just (d, c)) ds cs
