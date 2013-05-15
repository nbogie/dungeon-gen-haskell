module Graph (Graph(..), Edge(..), makeRandomGraph, trisToGraph, makeMST, makeMST2, summarizeGraph) where
import Types
import System.Random
import Triangulation
import Data.List (nub, (\\), find)
import Debug.Trace 
data Edge a = Edge (a, a) deriving (Eq, Show)

data Graph a = Graph { nodes :: [a]
                     , edges :: [Edge a]
                     } deriving (Show)

makeRandomGraph :: (RandomGen g) => g -> (Pos, Pos) -> Graph Pos
makeRandomGraph gen ((minX, minY), (maxX,maxY))= trisToGraph $ triangulate pts
  where
    (xs,ys) = splitAt 50 $ take 100 $ randomRs (minX, maxX) gen
    pts     = zip xs ys

trisToGraph :: [Triangle] -> Graph Pos
trisToGraph tris = Graph nodes edges
  where
    nodes = nub $ concatMap nodesInTri tris
    edges = nub $ concatMap edgesInTri tris
    nodesInTri (p,q,r) = [p,q,r]
    edgesInTri (p,q,r) = map Edge [(p,q), (q,r), (p, r)]


-- Make minimal spanning tree from given graph
-- Very inefficient implementation of Prim's algorithm, but fit for my purposes.
makeMST :: (Eq a) => Graph a -> Graph a
makeMST (Graph (n:ns) es) = connect (Graph [n] []) es
  where
    connect gr edgePool = case findEdge edgePool gr of
                            Just e  -> connect (add e gr) (edgePool \\ [e])
                            Nothing -> gr

-- incorrect.  may discard some entirely unconnected edges while looking for a half-in edge.
makeMST2 :: (Eq a) => Graph a -> Graph a
makeMST2 (Graph (n:ns) es) = connect (Graph [n] []) es
  where
    connect gr edgePool = case break (isHalfIn gr) edgePool of
                            (throwEs, (e:eRest))  -> connect (add e gr) eRest
                            (_,       [])         -> gr

add :: (Eq a) => Edge a -> Graph a -> Graph a
add e@(Edge (p1,p2)) (Graph ns es) = Graph (nub $p1:p2:ns) (e:es)

findEdge :: (Eq a) => [Edge a] -> Graph a -> (Maybe (Edge a))
findEdge es gr = find (isHalfIn gr) es

isHalfIn :: (Eq a) => Graph a -> Edge a -> Bool
isHalfIn (Graph ns _) (Edge (p1,p2)) = isIn p1 /= isIn p2
  where isIn p = p `elem` ns

summarizeGraph :: Graph a -> String
summarizeGraph (Graph ns es) = show ("Graph with ", length ns, " nodes and ", length es, " edges")

main = do
  gen <- getStdGen
  let graph = makeRandomGraph gen ((0,0),(10,20))
  let mst = makeMST graph
  print graph
  print mst
