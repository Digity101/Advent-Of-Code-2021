main :: IO ()
main = do
  measurments <- getContents
  let list  = map (read::String->Int) $ lines measurments
  let slidingWindow = zipWith3 (\x y z -> x+y+z) (tail $ tail list) (tail list) list 
  print $ sum $ zipWith (\x y -> if x >  y then 1 else 0) (tail slidingWindow) slidingWindow
  -- after seeing other solutions
  -- print $ sum $ zipWith (\x y -> if x >  y then 1 else 0) (drop 3 list) list