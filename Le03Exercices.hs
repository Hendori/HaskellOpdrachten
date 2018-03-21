module Le03Exercises where
-- Exercise 1
-- What are the types of the following values?
-- ['a', 'b', 'c']
-- [Char]
-- ('a', 'b', 'c')
-- ( Char,  Char,  Char)
-- [(False, '0'), (True, '1')]
-- [(Bool, Char)]
-- ([False, True], ['0', '1'])
-- ([Bool], [Char])
-- [tail, init, reverse]
-- [[a] -> [a]]
--
--
-- Exercise 2
bools :: [Bool]
bools = [True]

nums :: [[Int]]
nums = [[1,2],[3]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy a = (a, a)

apply :: (a -> b) -> a -> b

