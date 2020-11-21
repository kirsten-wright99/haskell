import Quiz

main :: IO ()
main = do
putStrLn("Which quiz would you like to take? Please input Space, Biology, or Math exactly as seen")
top <- getLine
putStrLn("For all questions, please answer 'True' or 'False'")
let quiz = getQuiz (read top::Topic)
putStrLn $ fst (quiz !! 0)
a1 <- getLine
putStrLn $ fst (quiz !! 1)
a2 <- getLine
let answers = [read a1::Bool, read a2::Bool]
putStrLn("Your score is " ++ show (getGrade quiz answers)) 