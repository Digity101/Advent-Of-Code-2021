import Data.List
import Data.Maybe

main :: IO ()
main = do
  input <- map ((\(a,b) -> (words a, words (tail b))) . (break ('|'==))) <$> lines <$> readFile "input.txt"
  --part 1
  print $ foldl (\x (a, b) -> x + countUnique b) 0 input
  
  --part 2
  print $ part2 input


countUnique :: [String] -> Int
countUnique xs = length $ filter (\x -> length x `elem` [2,4,3,7]) xs

part2 :: [([String], [String])] -> Int
part2 input =  foldl (\x y -> x + oneLine y) 0 input where
  oneLine (a, b) = read . concat $ map (toNumberString . (mapped $ correctPermutation a)) b where
    toNumberString x = show $ fromJust $ x `elemIndex` correct
    correct = [[0,1,2,4,5,6],[2,5],[0,2,3,4,6],[0,2,3,5,6],[1,2,3,5],[0,1,3,5,6],[0,1,3,4,5,6],[0,2,5],[0,1,2,3,4,5,6],[0,1,2,3,5,6]]
    mapped p s = sort $ map (\x -> fromJust (elemIndex x p)) s
    correctPermutation xs = head [ perm | perm <- permutations "deafgbc", valid perm] where
      valid perm = and $ map (checkSingle perm) xs
      checkSingle p x = mapped p x `elem` correct
      