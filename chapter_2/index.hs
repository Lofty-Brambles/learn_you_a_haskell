removeNonUppercase :: [Char] -> [Char]
removeNonUppercase w = [ c | c <- w, c `elem` ['A'..'Z'] ]

add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z