import Graphics.Triangulation.Delaunay
import System.Random
import Data.Vector.V2
import Graphics.Gloss.Interface.Pure.Display
import GHC.Float (double2Float, float2Double)
demoRandomTriangulation :: (RandomGen g) => g -> ([Vector2], [(Vector2, Vector2, Vector2)])
demoRandomTriangulation gen = (pts, triangulate pts)
  where
    (xs,ys) = splitAt 50 $ take 100 $ randomRs (-400,400) gen
    pts = zipWith Vector2 xs ys


main = do
  gen <- getStdGen
  let (pts, tris) = demoRandomTriangulation gen
  let makePic = Pictures [ 
                           Color red $ trisToPicture tris
                         , Color yellow $ dotsToPicture pts
                         ]
  display (FullScreen (700,700)) black makePic

type Tri = (Vector2, Vector2, Vector2)
type Pos = (Float, Float)
triToFloatTri :: Tri -> (Pos, Pos, Pos)
triToFloatTri (a,b,c) = (f a, f b, f c )
  where f = toFloatPos

trisToPicture :: [Tri] -> Picture
trisToPicture = Pictures . map triToPicture

triToPicture (p1,p2,p3) = Line (map toFloatPos [p1,p2,p3])

toFloatPos ::  Vector2 -> (Float, Float)
toFloatPos (Vector2 x y) = (double2Float x, double2Float y)

dotsToPicture :: [Vector2] -> Picture
dotsToPicture vs = Pictures $ map (\(Vector2 x y) -> translate (f x) (f y)$ rectangleSolid 2 2) vs
  where f = double2Float

