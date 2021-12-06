import Data.Array

-- ghci reports (1.30 secs, 523,433,504 bytes) for input.txt
main :: IO ()
main = do
    input <-  map (read::String->Int) . words . map (\c -> if c == ',' then ' ' else c) <$> readFile "input.txt"
    print $ foldl (\x y -> x + dynamicFish y 256) 0 input

dynamicFish :: Int -> Int -> Int
dynamicFish x y = dynamicFish'!(x,y)
    where
        dynamicFish' = array ((0,0),(8,y)) [ (xy, f xy) | xy <- range ((0,0),(8,y))]
        f (_, 0) = 1
        f (0, y') = dynamicFish'!(6,y'-1) + dynamicFish'!(8,y'-1)
        f (x',y') = dynamicFish'!(x'-1,y'-1)