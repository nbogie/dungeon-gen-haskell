module Graph ( Graph(..)
             , Edge(..)
             , makeRandomGraph
             , trisToGraph
             , makeMST
             , summarizeGraph
             , addEdgeToGraph
             , isIn
             , addSomeEdgesFrom
             , genRectiLines
             , Ln) where
import Data.List (nub, (\\), find)
import System.Random
import Triangulation

import Types
import Shuffle (shuffle)

data Edge a = Edge (a, a) deriving (Eq, Show)

data Graph a = Graph { gNodes :: [a]
                     , gEdges :: [Edge a]
                     } deriving (Show)

makeRandomGraph :: (RandomGen g) => g -> (Float, Float) -> Graph Pos
makeRandomGraph gen (mn, mx)= trisToGraph $ triangulate pts
  where
    (xs,ys) = splitAt 50 $ take 100 $ randomRs (mn,mx) gen
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
makeMST gr@(Graph [] _) = gr
makeMST (Graph (n:_) es) = connect (Graph [n] []) es
  where
    connect gr edgePool = case findEdge edgePool gr of
                            Just e  -> connect (add e gr) (edgePool \\ [e])
                            Nothing -> gr
    add = addEdgeToGraph
-- todo: use sets, or ensure no duplicate edges added here
addEdgeToGraph :: (Eq a) => Edge a -> Graph a -> Graph a
addEdgeToGraph e@(Edge (p1,p2)) (Graph ns es) = Graph (nub $p1:p2:ns) (e:es)

findEdge :: (Eq a) => [Edge a] -> Graph a -> (Maybe (Edge a))
findEdge es gr = find (isHalfIn gr) es

isHalfIn :: (Eq a) => Graph a -> Edge a -> Bool
isHalfIn (Graph ns _) (Edge (p1,p2)) = nodeIn p1 /= nodeIn p2
  where nodeIn p = p `elem` ns

isIn :: (Eq a) => Graph a -> Edge a -> Bool
isIn g e = e `elem` gEdges g

summarizeGraph :: Graph a -> String
summarizeGraph (Graph ns es) = show ("Graph with ", length ns, " nodes and ", length es, " edges")

addSomeEdgesFrom :: (RandomGen g, Eq a) => g -> Float -> Graph a -> Graph a -> Graph a
addSomeEdgesFrom gen pct superset orig = foldl (flip addEdgeToGraph) orig edgesToAdd
  where
    edgesToAdd = take (round (pct * fromIntegral (length unusedEdges))) unusedEdges 
    unusedEdges = fst $ shuffle gen $ filter (not . isIn orig) (gEdges superset)

type Ln = [Pos]

genRectiLines :: (Graph Pos) -> [Ln]
genRectiLines (Graph _ es) = map rectiLine es
  where
    rectiLine :: Edge Pos -> Ln
    rectiLine (Edge ((x1,y1), (x2,y2))) = [(x1,y1), (x1,y2), (x2,y2)]

_demo ::  IO ()
_demo = do
  gen <- getStdGen
  let graph = makeRandomGraph gen (0,20)
  let mst = makeMST graph
  print graph
  print mst
