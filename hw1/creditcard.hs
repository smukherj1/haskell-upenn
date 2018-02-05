toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n < 10 = [n]
toDigitsRev n = (n `mod` 10) : toDigitsRev (div n 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (n : []) = [2 * n]
doubleEveryOther (i : (j : [])) = [2 * i, j]
doubleEveryOther (i : (j : k)) = (2 * i) : (j : (doubleEveryOther k))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (n : []) = sum (toDigits n)
sumDigits (i : j) = sum [(sumDigits [i]), (sumDigits j)]

validate :: Integer -> Bool
validate n = result 
    where
        digitized = toDigits n
        doubled_digitized = doubleEveryOther digitized
        summed = sumDigits doubled_digitized
        result = (summed `mod` 10) == 0


main = do
    putStrLn "Hello World"
    let result = toDigits 1234567
    print result
    let result1 = doubleEveryOther result
    print result1
    let result2 = sumDigits [16, 7, 12, 5]
    print result2
    let result3 = validate 4012888888881881
    print result3
    let result4 = validate 4012888888881882
    print result4