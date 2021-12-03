import Data.Char

main :: IO ()
main = do
  input <- getContents
  let parsedInput = map (map digitToInt) $ lines input
  
  -- Part 1
  let gamma = binaryToInt $ count (<) parsedInput
  let epsilon = binaryToInt $ count (>) parsedInput
  print $ gamma * epsilon
  
  -- Part 2
  let co2 = binaryToInt $ part2 (parsedInput, 0, (<=))
  let o2 = binaryToInt $ part2 (parsedInput, 0, (>))
  print $ co2 * o2
  
count :: (Int -> Int -> Bool) -> [[Int]] -> [Bool]
count f input =  map ((f total).(*2)) $ foldl (zipWith (+)) (repeat 0) input where
  total = length input

binaryToInt :: [Bool] -> Int
binaryToInt = foldr (\x y -> fromEnum x + 2*y) 0 . reverse

part2 :: ([[Int]], Int, Int -> Int -> Bool) -> [Bool]
part2 ([x], _, _) = map (==1) x
part2 (xs,i, f) = part2 (filter (\x -> (x!!i) == j) xs, i + 1, f) where
  j = if (count f xs) !! i then 0 else 1
  