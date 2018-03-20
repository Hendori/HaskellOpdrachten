--Exercises hoofdstuk 2
module LE02Exercise where


--opgave 3
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]


--opgave 4
last' xs = head (reverse xs)


--opgave 5
init1 xs = reverse (tail (reverse xs))

init2 xs = take ((length xs) -1) xs
