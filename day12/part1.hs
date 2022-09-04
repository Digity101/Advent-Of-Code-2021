import Data.Graph
import Data.List
import Data.Maybe
import Data.Tree

main :: IO ()
main = do
    input <- map ((\(x,y) -> (x,tail y)) . break (=='-')) . lines <$> readFile "example.txt"
    let adj = buildAdjacencyList input
    let (graph, nodeFromVertex, vertexFromKey) = graphFromEdges adj
    let paths = dfs graph [fromJust $ vertexFromKey "start"]
    print $ paths

buildAdjacencyList :: [(String,String)] -> [(String, String, [String])]
buildAdjacencyList xs = mapped $ grouped $ bothDir xs where
    mapped xs = map (\x -> (fst (head x), fst (head x), concatMap (\(a,b) -> [b]) x)) xs
    grouped xs = nub $ groupBy (\(x,y) (x',y') -> x==x') xs
    bothDir xs = foldl (\x (a,b) -> x ++ [(a,b),(b,a)]) [] xs