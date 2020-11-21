import Data.List hiding (union)

type Set a = [a]

--helper function to remove duplicates
noDupes :: (Eq a) => [a] -> [a]
noDupes [] = []
noDupes (x:xs)
  |x `elem` xs = noDupes xs
  |otherwise = x : noDupes xs

--returns an empty set
empty :: Set a
empty = []

--returns true if the specified element is in the set
memberofSet :: Ord a => Set a -> a -> Bool
memberofSet x n = elem n x

--returns the union of two sets
union :: Ord a => Set a -> Set a -> Set a
union x y = sort(noDupes(x ++ y))

--returns the intersection of two sets
intersection :: Ord a => Set a -> Set a -> Set a
intersection x y = x `intersect` y

--returns a set containing the difference of two sets
difference :: Ord a => Set a -> Set a -> Set a
difference x [] = nub(sort(x))
difference [] y = nub(sort(y))
difference x y = sort ((nub x) \\ (nub y))

--returns true if two sets are equal
equalSet :: Ord a => Set a -> Set a -> Bool 
equalSet x y = length (difference (nub x) (nub y)) == 0

--returns true if the first set is a subset of the first
subSet :: Ord a => Set a -> Set a -> Bool 
subSet [] y = True
subSet x y = all (\x -> x `elem` y) x

mapSet :: Ord b => (a -> b) -> Set a -> Set b
mapSet f x = sort (nub(map f x))

filterSet :: Ord a => (a -> Bool) -> Set a -> Set a      
filterSet f x = filter f (nub (sort x))

foldSet :: (a -> a -> a) -> a -> Set a -> a
foldSet f x y = foldl f x y 

showSet :: Show a => Set a -> String 
showSet x = show x


main = do

  let s1 = empty
  let s2 = [1, 9, 4, 5, 2]
  let s3 = [1, 3, 7, 5, 2]
  let s4 = [1, 1, 3, 7, 6]
  let s5 = [1, 3, 7, 5]
  let s6 = [1, 3, 3, 5, 7]
  let s7 = [2, 4]

  putStrLn "Member of Set"
  print(memberofSet s1 5) -- False
  print(memberofSet s2 5) -- True
  print(memberofSet s4 9) -- False

  putStrLn " "
  putStrLn "Union"
  print(union s1 s2) -- [1,2,4,5,9]
  print(union s3 s4) -- [1,2,3,5,6,7]

  putStrLn " "
  putStrLn "Intersection"
  print(intersection s1 s4) -- []
  print(intersection s2 s5) -- [1,5]
  print(intersection s5 s6) -- [1,3,7,5]

  putStrLn " "
  putStrLn "Difference"
  print(difference s1 s2) -- [1,2,4,5,9]
  print(difference s4 s2) -- [3,7,6]

  putStrLn " "
  putStrLn "Equal Set"
  print(equalSet s1 s2) -- False
  print(equalSet s5 s6) -- True
  print(equalSet s4 s5) -- False

  putStrLn " "
  putStrLn "Sub Set"
  print(subSet s2 s1) -- False
  print(subSet s5 s6) -- True
  print(subSet s1 s2) -- True

  putStrLn " "
  putStrLn "Map Set"
  print(mapSet (* 4) s4) -- [4,12,24,28]
  print(mapSet (* 2) s2) -- [2,4,8,10,18]

  putStrLn " "
  putStrLn "Filter Set"
  print(filterSet (>2) s2) -- [4,5,9]
  print(filterSet (<9) s2) -- [1,2,4,5]

  putStrLn " "
  putStrLn "Fold Set"
  print(foldSet (/) 2 s7) -- 0.25
  print(foldSet (min) 2 s4) -- 1

  putStrLn " "
  putStrLn "Show Set"
  putStrLn(showSet s5)