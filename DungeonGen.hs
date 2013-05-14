module DungeonGen where
import Debug.Trace
import System.Random

type Pos = (Int, Int)
data Rect = Rect { rW::Int
                 , rH::Int
                 , rPos::Pos
                 } deriving (Show)

toXY :: Int -> Int -> (Float, Float)
toXY angle radius = undefined


-- inefficient, but we don't care just now
instance Random Rect where
  randomR lims@(Rect minW minH _, Rect maxW maxH _) gen = 
    if hasFatAspect r then (r,g'''') else randomR lims g''''
      where 
        r       = Rect w h pos
        (w,g')  = randomR (minW, maxW) gen
        (h,g'') = randomR (minH, maxH) g'
        angle, radius :: Int
        (angle,g''')   = randomR (0,359) g''
        (radius,g'''')  = randomR (0,400) g'''
        pos     = undefined -- toXY angle radius

  random = randomR (minRect, maxRect)
    where minRect = Rect 2 2 origin
          maxRect = Rect 40 40 origin
          origin = (0,0)

hasFatAspect :: Rect -> Bool
hasFatAspect r = (min ar (1/ar)) > 0.4
  where ar = aspectRatio r

genStartingRects gen n = let rects = take n $ randoms gen
                         in filter hasFatAspect rects
aspectRatio (Rect w h _) = (fromIntegral w)/(fromIntegral h)

main = do
  gen <- getStdGen
  print $ genStartingRects gen 10
