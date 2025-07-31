module Daylatro.Extra.Ord where

between :: (Ord a) => a -> a -> a -> Bool
between lo hi x = lo <= x && x <= hi
