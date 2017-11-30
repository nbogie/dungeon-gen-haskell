module Triangulation where
import Types
-- import qualified Graphics.Triangulation.Delaunay as DL
import qualified Delaunay as DL
import Data.Vector.V2
import GHC.Float (double2Float, float2Double)
import Data.List (nub)

type Triangle = (Pos, Pos, Pos)

triangulate :: [Pos] -> [Triangle]
triangulate pts = case nub pts of
                    (_:_:_) -> map triToFloatTri . DL.triangulate . map posToVector2  $ pts
                    _tooFew      -> []
  where
  triToFloatTri (a,b,c) = (f a, f b, f c )
  f = vector2ToPos
  vector2ToPos::  Vector2 -> (Float, Float)
  vector2ToPos (Vector2 x y) = (double2Float x, double2Float y)
  posToVector2 :: Pos -> Vector2
  posToVector2 (x,y) = Vector2 (float2Double x) (float2Double y)
