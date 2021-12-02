main :: IO ()
main = do
  measurments <- getContents
  let list  = map (\x -> (words x !! 0, (read::String->Int) $ words x !! 1) ) $ lines measurments
  print $ totalDirection "forward" list * (totalDirection "down" list - totalDirection "up" list)

totalDirection :: String -> [(String, Int)] -> Int
totalDirection keyword list = sum [ snd x | x <- list, fst x == keyword]
