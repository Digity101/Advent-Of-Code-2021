import Data.List
-- (2.16 secs, 592,765,088 bytes)
main :: IO ()
main = do
  input <- map (\x -> map ((read::String->Int).(:[])) x) . lines <$> readFile "input.txt"
  
  -- Part 1
  print $ sum $ map ((+1).at input) $ getLowpoints input
  
  -- Part 2
  print $ product . take 3 . reverse . sort . map (length . nub . getBasinAt input) $ getLowpoints input

at :: [[Int]] -> (Int, Int) -> Int
at xs (x,y) = (xs !! y) !! x

isLowPoint :: [[Int]] -> (Int, Int) -> Bool
isLowPoint xs (x,y) = and $ [isLowerAt (-1,0), isLowerAt (1,0),isLowerAt (0,-1),isLowerAt (0,1)] 
  where
    isLowerAt (a,b) 
      | (inBounds (x+a,y+b) xs) == (x,y) = True
      | otherwise = location < xs `at` (inBounds (x+a,y+b) xs)
    location = xs `at` (x,y)

inBounds :: (Int, Int) -> [[a]] -> (Int, Int)
inBounds (a,b) xs = (max 0 $ min a $ (length $ xs !! 0) - 1, max 0 $ min b $ -1 + length xs)

getLowpoints :: [[Int]] -> [(Int, Int)]
getLowpoints xs = [(a,b) | (a,b) <- cords, isLowPoint xs (a,b) ]
  where
    cords = [(x,y) | x <- [0..length (xs !! 0) - 1], y <- [0..length xs - 1]]

getBasinAt :: [[Int]] -> (Int, Int) -> [(Int, Int)]
getBasinAt xs (a,b) = getBasinAt' [(a,b)] where
  getBasinAt' cords
    | length cords == length (growBasin xs cords) = cords
    | otherwise = getBasinAt' $ growBasin xs cords

growBasin :: [[Int]] -> [(Int, Int)] -> [(Int, Int)]
growBasin xs cords = nub [(a,b) | (a,b) <- connected cords] where
  connected cords = cords ++ [ (x',y') | (x',y') <- concat $ map (surrounding xs) cords] where
  surrounding xs (x,y) = [(x', y') | (x', y') <- possible (x,y), guard (x', y')] where
    possible (x,y) = [inBounds (x-1,y) xs, inBounds (x+1,y) xs,inBounds (x,y+1) xs, inBounds (x,y-1) xs]
    guard (x', y') = (xs `at` (x',y') < 9) && (xs `at` (x',y') > xs `at` (x,y))