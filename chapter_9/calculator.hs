import System.Environment (getArgs)

main = do
  inputString <- getArgs
  let (stack : rest) = inputString
  print $ head $ reducer $ words stack

reducer :: (Fractional a, Read a) => [String] -> [a]
reducer = foldl eval []
  where
    eval (first : second : rest) "+" = (first + second) : rest
    eval (first : second : rest) "-" = (second - first) : rest
    eval (first : second : rest) "*" = (second * first) : rest
    eval (first : second : rest) "/" = (second / first) : rest
    eval all num = read num : all