import Data.Char (chr, ord)

-- caesar cipher
encode :: Int -> [Char] -> [Char]
encode shift message = map (chr . (+ shift) . ord) message