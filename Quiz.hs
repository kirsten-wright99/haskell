module Quiz (Topic(Space, Biology, Math), Question, Quiz, getGrade, getQuiz) where

data Topic = Space | Biology | Math deriving (Eq, Read, Show)
type Question = (String, Bool)
type Quiz = [Question]

getQuiz :: Topic -> Quiz
getQuiz Space = [("1) The largest planet in the solar system is Jupiter.", True), ("2) Saturn is the only planet in our solar system with rings.", False)]
qetQuiz Biology = [("1) The mitochondria is the powerhouse of the cell.", True), ("2) DNA stands for Dioxyribonucleic acid.", True)]
qetQuiz Math = [("Sir Isaac Newton invented Trigonometry", False), ("Ancient Egyptians considered the number 9 to be sacred", True)]

getGrade :: Quiz -> [Bool] -> Int
getGrade _ [] = 0
getGrade (a1:b1) (a2:b2) = (getGrade b1 b2) + if (snd a1 == a2) then 2 else -1