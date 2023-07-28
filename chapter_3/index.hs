lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

fibonacci :: (Integral a) => a -> [a]
fibonacci n
    | n <= 0 = error "no."
    | n == 1 = [0]
    | otherwise = 0 : 1 : zipWith (+) (fibonacci (n - 1)) (tail (fibonacci (n - 1)))