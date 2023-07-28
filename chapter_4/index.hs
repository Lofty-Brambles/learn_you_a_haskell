-- replicate' - where replicate 3 5 => [5,5,5]

replicate' :: (Num a, Ord a) => a -> b -> [b]
replicate' n p
  | n <= 0 = []
  | otherwise = p : replicate' (n - 1) p

-- take' - where take 3 [5, 4, 3, 2, 1] => [5, 4, 3]
take' :: (Num a, Ord a) => a -> [b] -> [b]
take' number list
  | number <= 0 = []
  | null list = []
  | otherwise = head : take' (number - 1) rest
  where
    (head : rest) = list

-- reverse - for a list
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

-- repeat' - repeats an element repeat' 's' => "sssss..."
repeat' :: z -> [z]
repeat' x = x : repeat' x

-- zip' - merges two lists together
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

-- elem' - detets an element in a list
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x : xs) = a == x || elem' a xs

