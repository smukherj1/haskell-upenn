type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 x y z = [(x, z)]
hanoi 2 x y z = [(x, y), (x, z), (y, z)]
hanoi n x y z = result
    where
        to_buffer = hanoi (n - 1) x z y
        move_bottom = [(x, z)]
        from_buffer = hanoi (n - 1) y x z
        result = (to_buffer ++ move_bottom) ++ from_buffer


main = do
    let result = hanoi 15 "a" "b" "c"
    print result