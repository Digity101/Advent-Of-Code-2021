import Data.List

type Octo = (Energy, Cord, Bool)
type Energy = Int
type Cord = (Int, Int)

main :: IO ()
main = do
    input <- map (\x -> map ((read::String->Int).(:[])) x) . lines <$> readFile "input.txt"
    let inputWithCord = zip3 (concat input) [(a,b) | a <- [0..9], b <- [0..9]] $ repeat False
    --Part1
    --print $ dumboOctopus inputWithCord 10 0
    
    --Part2
    print $ dumboOctopus2 inputWithCord 0

dumboOctopus :: [Octo] -> Int -> Int -> Int
dumboOctopus initial 0 flashes = flashes
dumboOctopus initial x flashes = dumboOctopus (step increased) (x - 1) (flashes + hasFlashed) where
    hasFlashed = length $ filter (\(x,_,_) -> x==0) initial
    increased = map (\(x,cord,flashed) -> (x+1, cord, False)) initial
    step state
        | map flashOnce state == state = state
        | otherwise = step $ map flashOnce state  
            where
                flashOnce (energy, cord, flashed)
                    | energy > 9 = (0, cord, True)
                    | flashed = (0, cord, True)
                    | otherwise = (energy + countAdjacents state (cord), cord, False)

dumboOctopus2 :: [Octo] -> Int -> Int
dumboOctopus2 initial x
    | allZero initial = x
    | otherwise = dumboOctopus2 (step increased) $ x + 1  
    where
        allZero state = and $ map (\(x,_,_) -> x==0) state
        hasFlashed = length $ filter (\(x,_,_) -> x==0) initial
        increased = map (\(x,cord,flashed) -> (x+1, cord, False)) initial
        step state
            | map flashOnce state == state = state
            | otherwise = step $ map flashOnce state  
                where
                    flashOnce (energy, cord, flashed)
                        | energy > 9 = (0, cord, True)
                        | flashed = (0, cord, True)
                        | otherwise = (energy + countAdjacents state (cord), cord, False)

at :: [Octo] -> Cord -> [Octo]
at xs (x,y) = filter (\(_,(a,b),_) -> a==x && b == y) xs

countAdjacents :: [Octo] -> Cord -> Int
countAdjacents xs (x,y) = length $ [(energy, (x',y'), flashed) | (energy, (x',y'), flashed) <- xs, (x',y') `elem` adjacents (x,y), energy > 9]

adjacents :: Cord -> [Cord]
adjacents (x,y) = delete (x,y) $ nub [bounded (x+m, y+n) | m <- [-1,0,1], n <- [-1,0,1], (m,n) /= (0,0)] where 
    bounded (a,b) = (max 0 $ min a 9, max 0 $ min b 9)

fstTriple :: (a,b,c) -> a
fstTriple (a,b,c) = a
