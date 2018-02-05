module Golf where

skipsN :: Int -> [t] -> [t]
skipsN _ [] = []
skipsN n ts = (drop (n - 1) (take n ts)) ++ (skipsN n (drop n ts))

skips :: [t] -> [[t]]
skips t = [skipsN k t | k <- [1..(length t)]]

main = do
    print (skips "ABCD")
    print (skips "hello!")
    print (skips [1])
    print (skips [True,False])
    print (length (skips []))