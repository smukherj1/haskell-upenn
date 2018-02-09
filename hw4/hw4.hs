module Hw4 where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' xs = foldl (\x y -> x * (y - 2)) 1 (filter even xs)

main = do
    let seq1 = [1, 4, 3, 6, 5, 8, 9, 10, 11, 12]
    let seq2 = [1, 3, 5, 7]
    let seq3 = [3]
    print (fun1 seq1)
    print (fun1' seq1)
    print (fun1 seq2)
    print (fun1' seq2)

