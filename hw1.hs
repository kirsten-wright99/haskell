--Kirsten Wright 
--COP4020 - Fall 2020 
import Data.List
import Data.Maybe
--Q1
--using recurision to delete from a list
delete_all_a :: (Eq a) => a -> ([a] -> [a])
delete_all_a y [] = []
delete_all_a y (x:xs)
  |  (x == y) = delete_all_a y xs
  |  otherwise = x : delete_all_a y xs

--using list comprehension to delete
delete_all_b x xs = [y | y <- xs, y /= x]

--using filter function to delete from list
delete_all_c x xs = filter (x/=) xs

--Q2
--Helper function that deletes the first element of a list
del_first :: (Eq a) => a -> ([a] -> [a])
del_first _ [] = [] 
del_first a (x:xs) 
  | a == x    = xs
  | otherwise = x : del_first a xs

-- Deletes the first and second element == a
del_fstnscnd :: (Eq a) => a -> ([a] -> [a])
del_fstnscnd _ [] = []
del_fstnscnd a (x:xs) 
  | x == a = del_first a xs
  | otherwise = x : del_fstnscnd a xs

--Q3
--takes a list of lists and merges them into one sorted list
merge :: (Ord a) => [[a]] -> [a]  
merge [] = []
merge x = (sort . concat) x

--helper function to remove duplicates
noDupes :: (Ord a) => [a] -> [a]
noDupes [] = []
noDupes (x:xs)
  |x `elem` xs = noDupes xs
  |otherwise = x : noDupes xs

--like merge but this one removes duplicates
mergeU :: (Ord a) => [[a]] -> [a]
mergeU [] = []
mergeU x = (noDupes . merge) x

--Q5
--Returns the sum of the harmonic series up until a given n
sumHarmonic :: (Eq a, Fractional a) => a -> a
sumHarmonic 1 = 1
sumHarmonic i = (1/i) + sumHarmonic (i-1)

--Q6
--helper function for the sum of cube
sumCube :: Int -> Int
sumCube 1 = 1
sumCube x = x^3 + sumCube (x - 1)

--helper function for the right side of the equation
funkyFunc :: Int -> Int
funkyFunc x = ((x * (x + 1)) `div` 2 )^2

--returns true if the left and right side of the equation are equal
evaluateFunc :: Int -> Bool 
evaluateFunc x = (sumCube x) == (funkyFunc x)

--Q7
--Takes a string and returns a list of the words in the string
string2word :: String -> ([String])
string2word s = words s

--Q8
--Finds a given element in a list
indexof :: (Eq a) => a -> [a] -> Int
indexof _ [] = -1
indexof x ls = fromMaybe (-1) $ elemIndex x ls

--Q9
--uses the newton approximation to get as close to a sqrt as possible
approximations :: (Real a, Fractional a) => a -> a -> [a]
approximations n a0 = iterate gTerm a0
    where
    gTerm x = ((x + n/x) / 2)

--Q10
--takes a finite list of functions and returns the composition
composeFunc :: [(a -> a)] -> (a -> a)
composeFunc [] = id
composeFunc xs = foldl (flip (.)) id xs


main = do
    putStrLn "HW1"
    putStrLn "Q1:Test"
    print(delete_all_a 1 [1, 2, 3, 2, 1, 2, 3, 2, 1])
    print(delete_all_b 3 [1, 2, 3, 2, 1, 2, 3, 2, 1])
    print(delete_all_c 2 [1, 2, 3, 2, 1, 2, 3, 2, 1])

    putStrLn " "
    putStrLn "Q2:Test"
    print(del_fstnscnd 1 [1, 2, 3, 2, 1, 2, 3, 2, 1])
    print(del_fstnscnd 4 [1, 2, 3, 2, 1, 2, 3, 2, 1, 4 ,4])

    putStrLn " "
    putStrLn "Q3: Test"
    print(merge [[1,3,5,7], [2,4,6], [3,5,9,10,11,12]])
    print(mergeU [[1,3,5,7], [2,4,6], [3,5,9,10,11,12]])

    putStrLn " "
    putStrLn "Q5: Test"
    print(sumHarmonic 7)
    print(sumHarmonic 20)

    putStrLn " "
    putStrLn "Q6: Test"
    print(evaluateFunc 1)
    print(evaluateFunc 2)
    print(evaluateFunc 3)
    print(evaluateFunc 4)
    print(evaluateFunc 5)
    print(evaluateFunc 6)
    print(evaluateFunc 7)
    print(evaluateFunc 8)
    print(evaluateFunc 9)
    print(evaluateFunc 10)
    print(evaluateFunc 11)
    print(evaluateFunc 12)
    print(evaluateFunc 13)
    print(evaluateFunc 14)
    print(evaluateFunc 15)
    print(evaluateFunc 16)
    print(evaluateFunc 17)
    print(evaluateFunc 18)
    print(evaluateFunc 19)
    print(evaluateFunc 20)

    putStrLn " "
    putStrLn "Q7: Test"
    print(string2word "This is a test")
    print(string2word "I don't like Haskell")
    
    putStrLn " "
    putStrLn "Q8: Test"
    print(indexof 8 [3, 5, 7, 9, 8, 10])
    print(indexof 2 [1,3,5,4,3,5])

    putStrLn " "
    putStrLn "Q9: Test"
    print(take 5 (approximations 2.0 1.0))
    print(take 5 (approximations 64.0 1.0))

    putStrLn " "
    putStrLn "Q10: Test"
    print(composeFunc [tail, init] [1, 2, 3, 4, 5])

