import qualified System.Environment as Env
import qualified System.IO.Error as IoError

main = do
    inputString <- Env.getArgs
    let (first:second:restOfInput) = inputString
        defaults = map read [first, second]
        result = head ( evaluate defaults restOfInput )
    putStrLn result

evaluate :: (Num a) => [a] -> [String] -> [a]
evaluate [] _ = []
evaluate result [] = result
evaluate (x:y:rest) (popped:restOfElements)
    | popped == "+" = evaluate ((x + y):rest) restOfElements
    | popped == "-" = evaluate ((y - x):rest) restOfElements
    | popped == "*" = evaluate ((x * y):rest) restOfElements
    | popped == "/" = evaluate ((y / x):rest) restOfElements
    | otherwise = evaluate ((read popped):x:y:rest) restOfElements
