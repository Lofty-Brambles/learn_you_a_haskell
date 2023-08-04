import System.Environment (getArgs)
import System.Exit (exitFailure)

main = do
  args <- getArgs
  if length args < 2
    then do
      putStrLn "Wrong input, please enter proper syntax `[command] \"(row, col)\" \"(row, col)\""
      exitFailure
    else print $ getPath [] [[read $ head args]] (read $ args !! 1)

type Position = (Int, Int)

getPath :: [Position] -> [[Position]] -> Position -> [Position]
getPath history (popped : queue) end
  | head popped == end = reverse popped
  | otherwise = getPath newHistory (queue ++ newQueues) end
  where
    newPositions = generateMoves history $ head popped
    newHistory = newPositions ++ history
    newQueues = map (: popped) newPositions

generateMoves :: [Position] -> Position -> [Position]
generateMoves history (row, col) = filter checkShifts . filter squareExists $ map mapShifts shifts
  where
    shifts = [(1, 2), (1, -2), (-1, 2), (-1, -2), (2, 1), (2, -1), (-2, 1), (-2, -1)]
    mapShifts (dx, dy) = (row + dx, col + dy)
    checkShifts (row', col') = row' `elem` [1 .. 8] && col' `elem` [1 .. 8]
    squareExists square' = square' `notElem` history