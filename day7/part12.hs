main :: IO ()
main = do
    input <-  map (read::String->Int) . words . map (\c -> if c == ',' then ' ' else c) <$> getLine
    print $ bruteForceFuel input
    print $ bruteForceFuel2 input

bruteForceFuel :: [Int] -> Int
bruteForceFuel xs = minimum $ map (\z -> f z) [minimum xs..maximum xs] where
  f n = foldl (\x y -> x + abs(y-n)) 0 xs

bruteForceFuel2 :: [Int] -> Int
bruteForceFuel2 xs = minimum $ map (\z -> f z) [minimum xs..maximum xs] where
  f n = foldl (\x y -> x + sumConsective (abs (y-n))) 0 xs
  sumConsective x = (x*(x + 1)) `div` 2
