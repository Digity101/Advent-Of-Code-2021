import Data.List.Split
import Data.List
import Data.Ord

type Board = [[(Bool, String)]]

main :: IO ()
main = do
  drawnNumbers <- splitOn "," <$> getLine
  boards <- parseBoards . lines <$> getContents
  --part 1
  print $ score $ playBingo (boards, drawnNumbers, "", Nothing)
  
  --part 2
  let (_,_,c,d) = playBingo2 (boards, drawnNumbers, [], [])
  let numbersToDraw = fst $ splitAt (last $ elemIndices (last c) drawnNumbers) drawnNumbers
  print (c,d)
  let newBoards = getBoard boards $ numbersToDraw++[last c]
  print $ (newBoards, last c, Just 0)
  print $ score (newBoards,[],last c, Just 0)

parseBoards :: [String] -> [Board] 
parseBoards xs = chunksOf 5 $ filter (/=[]) $ map (zip (repeat False). words) xs

placeNumber :: String -> Board -> Board
placeNumber n xs = (map . map) (\(a,b) -> (b==n || a, b)) xs

split5 :: [a] -> [[a]]
split5 [] = [[]]
split5 (a:b:c:d:e:xs) = [a,b,c,d,e]:(split5 xs)

checkRow :: [(Bool, String)] -> Bool
checkRow xs = foldl (&&) True $ map fst xs

checkBoard :: Board -> Bool
checkBoard xs = foldl (||) False colAndRow where
  colAndRow = (map checkRow xs) ++ (map checkRow $ transpose xs)

playBingo :: ([Board],[String], String, Maybe Int) ->  ([Board],[String], String, Maybe Int) 
playBingo (a, [], c, d) = (a, [], c, d)
playBingo (a, b, c, Just d) = (a, b, c, Just d)
playBingo (boards, (x:xs), _,Nothing) = playBingo (newBoard, xs, x, z) where
  newBoard = map (placeNumber x) boards
  z = findIndex id (map checkBoard newBoard)

playBingo2 :: ([Board],[String], [String], [Int]) ->  ([Board],[String], [String], [Int]) 
playBingo2 (a, [], c, d) = (a, [], c, d)
playBingo2 (boards, (x:xs), y, p) = playBingo2 (newBoard, xs, q, z) where
  newBoard = map (placeNumber x) boards
  z = findIndices id (map checkBoard newBoard)
  q = if length z > length p then y++[x] else y

getBoard :: [Board] -> [String] -> [Board]
getBoard x [] = x
getBoard [a] (x:xs) = map (placeNumber x) [a]
getBoard a (x:xs) = getBoard boards xs where
  boards = filter (not . checkBoard) (map (placeNumber x) a)

score :: ([Board],[String], String, Maybe Int) -> Int
score (a,b,c,d) = unmarked * calledNumber where
  unmarked = sum $ map (read.snd) $ filter (\(x,y)-> not x) $ concatMap id $ a !! (maybe 0 id d)
  calledNumber = read c
