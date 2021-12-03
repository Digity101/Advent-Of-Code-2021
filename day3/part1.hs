import Data.Char

main :: IO ()
main = do
  input <- getContents
  let parsedInput = map (map digitToInt) $ lines input
  let gamma = binaryToInt $ count (<) parsedInput
  let epsilon = binaryToInt $ count (>) parsedInput
  print $ gamma * epsilon

count :: (Int -> Int -> Bool) -> [[Int]] -> [Bool]
count f input = map ((f total).(*2)) $ foldl (zipWith (+)) (repeat 0) input where
  total = length input

binaryToInt :: [Bool] -> Int
binaryToInt = foldr (\x y -> fromEnum x + 2*y) 0 . reverse