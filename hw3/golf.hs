module Golf where

skipsN :: Int -> [t] -> [t]
skipsN _ [] = []
skipsN n ts = (drop (n - 1) (take n ts)) ++ (skipsN n (drop n ts))

skips :: [t] -> [[t]]
skips t = [skipsN k t | k <- [1..(length t)]]

localMaxima :: [Integer] -> [Integer]
localMaxima ls = map (\(x, y, z) -> y) (filter (\(x, y, z) -> (y > x) && (y > z)) (zip3 ls (drop 1 ls) (drop 2 ls)))

histogram :: [Int] -> String
histogram _ = ""

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
    print (histogram histogram_data)