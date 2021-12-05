import Data.List.Split
import Data.List

main :: IO ()
main = do
  input <- readCords <$> map (map (splitOn ",") . words) . lines <$> getContents
  print $ part1 input
  print $ part2 input

readCords :: [[[String]]]-> [((Int, Int), (Int, Int))]
readCords xs = [ ((read x1, read y1), (read x2, read y2)) | [[x1,y1],_,[x2,y2]] <- xs]

count2LineOverlap :: [(Int, Int)] -> Int
count2LineOverlap xs = length $ filter (>=2) $ map length $ group $ sort xs         

part1 :: [((Int, Int), (Int, Int))] -> Int
part1 xs = count2LineOverlap input
    where
      input = collectPoints $ filterLines xs
      collectPoints = foldl (\x y -> x ++ coverPoints y) []
      coverPoints ((x1,y1),(x2,y2)) = [(x,y) | x <-[a..b], y <- [c..d]] 
        where
          (a,b) = if x1 < x2 then (x1,x2) else (x2,x1)
          (c,d) = if y1 < y2 then (y1,y2) else (y2,y1)
      filterLines = filter (\((x1,y1),(x2,y2)) -> x1 == x2 || y1 == y2)

part2 :: [((Int, Int), (Int, Int))] -> Int      
part2 xs = count2LineOverlap input
    where
      slope (x1,y1) (x2,y2) = abs (y1-y2) == abs (x1-x2)
      input = collectPoints $ filterLines xs
      collectPoints = foldl (\x y -> x ++ coverPoints y) []
      coverPoints ((x1,y1),(x2,y2))
        | x1==x2 || y1==y2 = [(x,y) | x <-[a..b], y <- [c..d]]
        | slope (x1,y1) (x2,y2) = [(x,y) | x <-[a..b], y <- [c..d], slope (x1,y1) (x,y)]
        where
          (a,b) = if x1 < x2 then (x1,x2) else (x2,x1)
          (c,d) = if y1 < y2 then (y1,y2) else (y2,y1)
      filterLines = filter (\((x1,y1),(x2,y2)) -> x1 == x2 || y1 == y2 || slope (x1,y1) (x2,y2)) 