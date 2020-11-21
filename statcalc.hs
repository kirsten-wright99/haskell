import Stats

parse_int :: String -> [Int]
parse_int xs = map read (words xs)

main :: IO()
main = do
putStrLn("Please enter a list of 10 integers on the range (1-100) with spaces between them.")
intstring <- getLine
let dataSet = parse_int intstring

putStr("Mean: ")
print(mean dataSet)
putStrLn("")

putStr("Mode: ")
print(mode dataSet)
putStrLn("")

putStr("Maximum: ")
print(maximum dataSet)
putStrLn("")

putStr("Minimum: ")
print(minimum dataSet)
putStrLn("")

putStr("Range: ")
print(range dataSet)
putStrLn("")