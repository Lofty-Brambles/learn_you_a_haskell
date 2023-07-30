import Data.Char (isDigit)
import Data.List (null)

main = do
  inputString <- getContents
  let segments = lines inputString
  let (valid, reason) = inputValidator segments
  if not valid
    then print reason
    else putStr $ processShortestPath segments

inputValidator :: [String] -> (Bool, String)
inputValidator inputs
  | not $ holdsForAll (holdsForAll digitCheck) inputs = (False, "The inputs aren't all valid distances!")
  | mod (length inputs) 3 /= 0 = (False, "All input paths aren't given")
  | otherwise = (True, "Validation passes!")
  where
    digitCheck x = isDigit x || x == '.'

holdsForAll :: (a -> Bool) -> [a] -> Bool
holdsForAll condition = foldr (\x isValid -> isValid && condition x) True

processShortestPath :: [String] -> String
processShortestPath segments = prefix ++ show shortestPath ++ pricePrefix ++ show dist
  where
    -- processShortestPath segments = prefix ++ show (getPaths [] streets)

    streets = groupIn3 [] $ map (\x -> read x :: Float) segments
    (shortestPath, dist) = getShortestPath $ getPaths [] streets
    prefix = "The best path to take is: "
    pricePrefix = "\nThe price for that is: "

groupIn3 :: (Floating a) => [(a, a, a)] -> [a] -> [(a, a, a)]
groupIn3 groups [] = groups
groupIn3 groups (x : y : z : segments)
  | null groups = groupIn3 [(x, y, z)] segments
  | otherwise = groupIn3 ((x, y, z) : groups) segments

data PathType = Top | Bottom | Change deriving (Show, Eq)

data Path = Path {kind :: PathType, distance :: Float}

instance Show Path where
  show :: Path -> String
  show (Path kind dist) = "( " ++ show kind ++ " -> " ++ show dist ++ " )"

getPaths :: [[Path]] -> [(Float, Float, Float)] -> [[Path]]
getPaths paths [] = paths
getPaths [] ((t, b, c) : groups) = getPaths use groups
  where
    (minimum, maximum, both) = getInitialDiff t b
    use = if null both then [[minimum]] else [[maximum], [minimum]]
getPaths paths ((t, b, c) : groups) = getPaths use groups
  where
    use = getDiff paths t b c

getInitialDiff :: Float -> Float -> (Path, Path, [Path])
getInitialDiff top bottom
  | top < bottom = (topPath, bottomPath, [])
  | top > bottom = (bottomPath, topPath, [])
  | otherwise = (topPath, bottomPath, [topPath, bottomPath])
  where
    topPath = Path {kind = Top, distance = top}
    bottomPath = Path {kind = Bottom, distance = bottom}

-- the problem
getDiff :: [[Path]] -> Float -> Float -> Float -> [[Path]]
getDiff paths@((e : _) : _) top bottom change
  | diff > 0 = map (\x -> oppPath : changePath : x) paths
  | diff < 0 = map (samePath :) paths
  | diff == 0 = concatMap (\x -> [oppPath : changePath : x, samePath : x]) paths
  where
    (Path lastKind _) = e
    (samePath, oppPath) = getKinds lastKind top bottom
    (Path kindOfSame distOfSame) = samePath
    (Path kindOfOpp distOfOpp) = oppPath
    changePath = Path {kind = Change, distance = change}
    diff = distOfSame - change - distOfOpp

getKinds :: PathType -> Float -> Float -> (Path, Path)
getKinds kind top bottom
  | kind == Top = (topPath, bottomPath)
  | kind == Bottom = (bottomPath, topPath)
  where
    topPath = Path {kind = Top, distance = top}
    bottomPath = Path {kind = Bottom, distance = bottom}

getShortestPath :: [[Path]] -> ([Path], Float)
getShortestPath paths = foldr1 getMinimum $ map getDistance paths
  where
    getDistance x = (x, foldr (\(Path _ dist) acc -> acc + dist) 0 x)
    getMinimum all1@(name1, dist1) all2@(name2, dist2)
      | dist1 >= dist2 = all1
      | otherwise = all2