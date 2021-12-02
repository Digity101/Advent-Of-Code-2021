main :: IO ()
main = do
  measurments <- getContents
  let list  = map (\x -> (words x !! 0, (read::String->Int) $ words x !! 1) ) $ lines measurments
  print $ totalDirection "forward" list * totalDirection "aim" list

totalDirection :: String -> [(String, Int)] -> Int
totalDirection "aim" list = sum [ snd x * (aim i list)  | (i,x) <- zip [1..] list, fst x == "forward"]
totalDirection keyword list = sum [ snd x | x <- list, fst x == keyword]

aim :: Int -> [(String, Int)] -> Int
aim index list = totalDirection "down" (take index list) - totalDirection "up" (take index list)