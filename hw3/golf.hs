module Golf where

skipsN :: Int -> [t] -> [t]
skipsN _ [] = []
skipsN n ts = (drop (n - 1) (take n ts)) ++ (skipsN n (drop n ts))

skips :: [t] -> [[t]]
skips t = [skipsN k t | k <- [1..(length t)]]

extractLM :: [Integer] -> [Integer]
extractLM (x:(y:(z:[])))
	| (y > x) && (y > z) = [y]
extractLM _ = []

localMaxima :: [Integer] -> [Integer]
localMaxima ls = foldl (++) [] (map extractLM [ take 3 (drop i ls) | i <- [0..(length(ls)-3)]])

main = do
    print (skips "ABCD")
    print (skips "hello!")
    print (skips [1])
    print (skips [True,False])
    print (length (skips []))
    print (localMaxima [2,9,5,6,1])