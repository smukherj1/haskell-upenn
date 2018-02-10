module Hw4 where

import Data.List

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' xs = foldl (\x y -> x * (y - 2)) 1 (filter even xs)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

treeDepth :: Tree a -> Integer
treeDepth Leaf = -1
treeDepth (Node d _ _ _) = d

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree [x] = Node 0 Leaf x Leaf
foldTree (x:xs) = result where
    half_subtree_size = (length xs) `div` 2
    left_subtree = foldTree (take half_subtree_size xs)
    right_subtree = foldTree (drop half_subtree_size xs)
    d = (maximum [treeDepth left_subtree, treeDepth right_subtree]) + 1
    result = Node d left_subtree x right_subtree

xor :: [Bool] -> Bool
xor xs = odd (length (filter id xs))

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f xs = foldr (\a as -> f a : as) [] xs

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = result where
    ns = [1..n]
    ijs = foldl (++) [] [zip (repeat i) [i..((n-i) `div` (2*i+1))] | i <- ns]
    blklist = sort ([0] ++ (map (\(x, y) -> (x + y + (2 * x * y))) ijs))
    prime_ranges = [[(i+1)..(j-1)] | (i, j) <- (zip blklist (drop 1 blklist))]
    result = foldl (++) [] [map (\x-> 2*x+1) xs | xs <- prime_ranges]


main = do
    let seq1 = [1, 4, 3, 6, 5, 8, 9, 10, 11, 12]
    let seq2 = [1, 3, 5, 7]
    let seq3 = [3]
    print (fun1 seq1)
    print (fun1' seq1)
    print (fun1 seq2)
    print (fun1' seq2)
    print (foldTree "ABCDEFGHIJ")
    print (xor [True, False, False, False] == True)
    print (xor [True, True, False, False, False] == False)
    print (map' (*2) [1, 2, 3, 4, 5, 6])
    print (length (sieveSundaram 100000))
