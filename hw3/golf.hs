module Golf where

-- Begin skips
skipsN :: Int -> [t] -> [t]
skipsN _ [] = []
skipsN n ts = (drop (n - 1) (take n ts)) ++ (skipsN n (drop n ts))

skips :: [t] -> [[t]]
skips t = [skipsN k t | k <- [1..(length t)]]
-- End skips

-- Begin Local maxima from a list of integers
localMaxima :: [Integer] -> [Integer]
localMaxima ls = map (\(x, y, z) -> y) (filter (\(x, y, z) -> (y > x) && (y > z)) (zip3 ls (drop 1 ls) (drop 2 ls)))
-- End Local maxima from a list of integers

-- Begin histogram
getCount :: Int -> [Int] -> Int
getCount n xs = length (filter (n==) xs)

getCounts :: [Int] -> [Int]
getCounts xs = [getCount i xs | i <- [0..9]]

getLevelStr :: Int -> [Int] -> String
getLevelStr n xs =  [ if (getCount i xs) >= n then '*' else ' '  | i <- [0..9]]

histogram :: [Int] -> String
histogram xs = unlines (graph ++ footer) where
    graph = [ getLevelStr i xs | i <- reverse [1..maximum (getCounts xs)]]
    footer = [['=' | _ <- [0..9]], (foldl (++) "" [show i | i <- [0..9]])]
-- End histogram

main = do
    print (skips "ABCD")
    print (skips "hello!")
    print (skips [1])
    print (skips [True,False])
    print (length (skips []))
    let ls = [2,9,5,6,1,7,3,5,6,7,8,9,6]
    print (ls)
    print (localMaxima ls)

    let histogram_data = [1,4,5,4,6,6,3,4,2,4,9]
    print (getCounts histogram_data)
    putStr (histogram histogram_data)
    putStr (histogram [1,1,1,5])