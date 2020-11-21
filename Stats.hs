module Stats (mean, mode, modes, range) where  

import Data.List
import Data.Ord
import System.IO
import System.Exit

mean :: [Int] -> Double
mean xs = realToFrac $ sum xs `div` genericLength xs

mode :: (Ord a) => [a] -> Maybe a
mode xs = case m of
            [] -> Nothing
            _  -> Just . snd $ head m
    where m = filter (\(a,b) -> a > 1) (modes xs) 

modes :: (Ord a) => [a] -> [(Int, a)]
modes xs = sortBy (comparing $ negate.fst) $ map (\x->(length x, head x)) $ (group.sort) xs

range :: (Real a) => [a] -> a
range x = maximum x - minimum x