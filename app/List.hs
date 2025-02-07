module List (main) where

a::[Integer]
a=[1,2,3,4,5,6,7,8,9,10]

elem'::(Eq a)=>a->[a]->Bool
elem' _ [] = False
elem' z (x:xs) = x==z || elem' z xs

nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs) = if elem' x xs then nub xs else x:nub xs

isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [x] = True
isAsc (x:y:xs) = x <= y && isAsc (y:xs)

isMove :: [(Int, Int)] -> Int -> Bool
isMove [] _ = False
isMove ((x, _):xs) a = a == x || isMove xs a

hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath [] _ _ = False
hasPath ((x, y):xs) a b
    | x == a && y == b = True
    | a == x = hasPath xs y b
    -- | b == y = hasPath xs a x
    | otherwise = isMove xs a && hasPath (xs ++ [(x, y)]) a b

hasPath' :: [(Int, Int)] -> Int -> Int -> Bool
hasPath' [] x y = x == y
hasPath' xs x y
  | x == y = True
  | otherwise =
    let xs' = [ (n, m) | (n, m) <- xs, n /= x] in
        or [ hasPath' xs' m y | (n, m) <- xs, n == x]

sum' :: [Int] -> Int
sum' = foldr (+) 0

rev' :: [a] -> [a]
rev' = foldl (\x z -> z:x) []

rev'' :: [a] -> [a]
rev'' = foldl (flip (:)) []

-- Create a function that will return prefixes
-- i.e prefixes [1, 2, 3] => [[1], [1, 2], [1, 2, 3]]
prefixes :: [a] -> [[a]]
prefixes y = rev'' $ foldl (\x z -> (take (length x) y++[z]) :x) [] y
prefixes' = foldr(\x acc -> [x] : (map ((:) x) acc)) []

main :: IO ()
main = do
    print (elem' 3 [1,2,3])
    print (nub [1, 2, 3, 3])
    print (isAsc [1, 2, 3, 4])
    print (hasPath [(1, 2), (2, 3), (3, 2), (4, 3), (4, 5)] 5 1)
    print (hasPath' [(1, 2), (2, 3), (3, 2), (4, 3), (4, 5)] 5 1)
    print (sum' [1, 2, 3, 4])
    print $ rev' [1,2,3]
    print $ rev'' [1,2,3]
    print (prefixes [1, 2, 3])
    print (prefixes' [1, 2, 3])