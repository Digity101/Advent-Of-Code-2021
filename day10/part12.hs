import Data.List

main :: IO ()
main = do 
  input <- lines <$> readFile "input.txt"
  -- Part 1
  print $ sum $ map (toPoints . getIllegalToken) input
  
  -- part 2
  print $ median $ map (toScore . autoComplete) $ filterIncomplete input

getIllegalToken :: String -> Char  
getIllegalToken s  = last . fst . break (=='1') $ parse s 
 
parse :: String -> String 
parse s = foldl parse' "" s where
  parse' parsedSoFar token
    | token == ')' = if last parsedSoFar == '(' then init parsedSoFar else parsedSoFar ++ [token] ++ ['1']
    | token == ']' = if last parsedSoFar == '[' then init parsedSoFar else parsedSoFar ++ [token] ++ ['1']
    | token == '}' = if last parsedSoFar == '{' then init parsedSoFar else parsedSoFar ++ [token] ++ ['1']
    | token == '>' = if last parsedSoFar == '<' then init parsedSoFar else parsedSoFar ++ [token] ++ ['1']
    | otherwise = parsedSoFar ++ [token]
    
toPoints :: Char -> Int
toPoints x
  | x == ')' = 3
  | x == ']' = 57
  | x == '}' = 1197
  | x == '>' = 25137
  | otherwise = 0

filterIncomplete :: [String] -> [String]
filterIncomplete xs = filter (notElem '1' ) $ map parse xs

autoComplete :: String -> [Int]
autoComplete s = reverse $ map complete s where
  complete token
    | token == '(' = 1
    | token == '[' = 2
    | token == '{'= 3
    | token == '<'= 4
    | otherwise = 0
    
toScore :: [Int] -> Int
toScore xs = foldl ((+).(*5)) 0 xs

median :: [Int] -> Int
median xs = sort xs !! (length xs `div` 2)