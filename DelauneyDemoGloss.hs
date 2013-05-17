module Main where

import Graphics.Triangulation.Delaunay
import System.Random
import System.Environment (getArgs)
import Data.Vector.V2
import Graphics.Gloss.Interface.Pure.Display
import GHC.Float (double2Float, float2Double)
import Shuffle (shuffle)
import Text.Show.Pretty (ppShow)

demoRandomTriangulation :: (RandomGen g) => g -> ([Vector2], [(Vector2, Vector2, Vector2)])
demoRandomTriangulation gen = (pts, triangulate pts)
  where
    (xs,ys) = splitAt 50 $ take 100 $ randomRs (-400,400) gen
    pts = zipWith Vector2 xs ys

unknownOrder  = [(-250,0),(-15,250),(-30,30),(-50,-75),(-150,50)]
crossingOrder = [(-250,0),(-50,-75),(-15,250),(-150,50),(-30,30)]
partialOrder  = [(-150,50),(-30,30),(-50,-75),(-15,250),(-250,0)]

missingPointPts = [(-30,30),(-150,50),(-50,-75),(-250,0),(-15,250)]

demoProblem2 = (pts, triangulate pts)
  where pts = map (posToVector2) missingPointPts

demoProblem3 = (pts, triangulate pts)
  where pts = map posToVector2 incompleteTrianglePts

incompleteTrianglePts = [(-250,0),(-15,250),(-150,50),(-50,-75),(-30,30)]

demoProblem1 :: (RandomGen g) =>g ->  ([Vector2], [(Vector2, Vector2, Vector2)])
demoProblem1 gen = (badPoints, triangulate badPoints)
  where 
    -- badPoints = map (posToVector2 . mult 5) [(-3,48),(-6,6),(-34,11),(-11,-15),(-54,2)]
    (badPoints, _g) = shuffle gen (map (posToVector2) [(-15,250),(-30,30),(-150,50),(-50,-75),(-250,0)])

mult n (x,y) = (x*n, y*n)

main = do
  gen <- getStdGen
  [caseNum] <- getArgs
  let demo = case (read caseNum)::Int of
              1 -> demoProblem1 gen
              2 -> demoProblem2
              3 -> demoProblem3
              4 -> demoRandomTriangulation gen
              _ -> error "unknown case number in main"

  let (pts, tris) = demo
  putStrLn $ ppShow $ ( "triangulating points"
          , map vector2ToIntPos pts
          , "triangles"
          , map triToIntTri tris
          )
  let makePic = Pictures [ 
                           Color red $ trisToPicture tris
                         , Color yellow $ dotsToPicture pts
                         ]
  display (InWindow "Delauney" (700,700) (10,10)) black makePic

type Tri = (Vector2, Vector2, Vector2)
type Pos = (Float, Float)

triToFloatTri :: Tri -> (Pos, Pos, Pos)
triToFloatTri (a,b,c) = (f a, f b, f c )
  where f = vector2ToFloatPos

triToIntTri vtri = let (p1,p2,p3) = triToFloatTri vtri in (f p1, f p2, f p3) 
  where f (x,y) = (round x, round y)

trisToPicture :: [Tri] -> Picture
trisToPicture = Pictures . map triToPicture

triToPicture (p1,p2,p3) = Line (map vector2ToFloatPos [p1,p2,p3,p1])

vector2ToFloatPos ::  Vector2 -> (Float, Float)
vector2ToFloatPos (Vector2 x y) = (double2Float x, double2Float y)
vector2ToIntPos :: Vector2 -> (Int, Int)
vector2ToIntPos v2 = let (x,y) = vector2ToFloatPos v2 in (round x, round y)
posToVector2 :: (Float, Float) -> Vector2
posToVector2 (x,y) = Vector2 (float2Double x) (float2Double y)

dotsToPicture :: [Vector2] -> Picture
dotsToPicture vs = Pictures $ map (\(Vector2 x y) -> translate (f x) (f y)$ rectangleSolid 4 4) vs
  where f = double2Float
