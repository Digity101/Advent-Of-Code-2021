main :: IO ()
main = do
  measurments <- getContents
  let list  = map (read::String->Int) $ lines measurments
  print $ sum $ zipWith (\x y -> if x >  y then 1 else 0) (tail list) list
