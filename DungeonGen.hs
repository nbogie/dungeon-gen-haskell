module DungeonGen where
import Debug.Trace
import System.Random
import Data.List (foldl')
type Pos = (Float, Float)
data Rect = Rect { rW::Float
                 , rH::Float
                 , rPos::Pos
                 } deriving (Show)

toXY :: Float -> Float -> (Float, Float)
toXY angle radius = (radius * sin angle, radius * cos angle)

type Vel = (Float, Float)

data Particle a = Particle {pPos::Pos, pVel:: Vel, pRad :: Float, pContent:: a} deriving (Show)

rectsToParticles :: [Rect] -> [Particle Rect]
rectsToParticles = map (\rect@(Rect w h pos) -> Particle pos zeroVel (max w h) rect)

updateVels :: [Particle a] -> [Particle a]
updateVels ps = map (flip updateVelOf ps) ps -- TODO: don't compare to self
-- TODO: add drag

snapParticlePositions n = map (snapParticlePosition n)

snapParticlePosition :: Int -> Particle a -> Particle a
snapParticlePosition n (Particle p v r c) = (Particle p' v r c)
  where
    p' = snapPos n p

snapPos tolerance (x,y) = (f x, f y)
  where 
   f n = t * (fromIntegral (round (n / t)))
   t   = fromIntegral tolerance

addVec (x,y) (a,b) = (x+a, y+b)
minVel = 0.1
updateParticles ps = updatePosns $ updateVels ps
updatePosns = map updatePosn
updatePosn (Particle (x,y) (vx, vy) r c) = (Particle (x+vx, y+vy) (vx,vy) r c)
modVel f (Particle p v r c ) = (Particle p (f v)  r c)
applyDrag p = modVel (f) p
  where f v = if lenVec v < minVel then (0,0) else multVecScalar v 0.7
updateVelOf :: Particle a -> [Particle a] -> Particle a
updateVelOf p@(Particle pos vel rad content) others = applyDrag $ (Particle pos (addVec vel velOffset) rad content)
  where
    velOffset = (`divideVec` (fromIntegral numClosePs)) $ foldl' push (0,0) closePs
      where
        push (vx, vy) pOther = (vx + forceX , vy+forceY)
          where (forceX, forceY) = applyForce (0.3*(distance pos (pPos pOther))) (directionFromTo (pPos pOther) pos)
    closePs    = filter (isClose p) others
    numClosePs = length closePs

applyForce force dir =  multVecScalar dir force
multVecScalar (vx,vy) force = (vx * force, vy * force)

lenVec v = distance (0,0) v

normalize v@(x,y) = if lenVec v == 0 then v else divideVec v (lenVec v)
directionFromTo (x1,y1) (x2,y2) = normalize (x2-x1, y2-y1)
divideVec (x,y) d = (x/d, y/d)
negateVec (x,y) = (negate x, negate y)

isClose p1 p2 = dist < ((r1 + r2)/2)
  where
    dist = distance (pPos p1) (pPos p2)
    r1 = pRad p1
    r2 = pRad p2

zeroVel = (0,0)
distance (x1,y1) (x2,y2) = sqrt (deltaX ^2 + deltaY^2)
  where deltaX = x1 - x2
        deltaY = y1 - y2
-- inefficient, but we don't care just now
instance Random Rect where
  randomR lims@(Rect minW minH _, Rect maxW maxH _) gen = 
    if hasFatAspect r then (r,g'''') else randomR lims g''''
      where 
        r       = Rect w h pos
        (w,g')  = randomR (minW, maxW) gen
        (h,g'') = randomR (minH, maxH) g'
        angle, radius :: Float
        (angle,g''')   = randomR (0,359) g''
        (radius,g'''')  = randomR (0,100) g'''
        pos     = toXY angle radius

  random = randomR (minRect, maxRect)
    where minRect = Rect 6 6 origin
          maxRect = Rect 60 60 origin
          origin = (0,0)

hasFatAspect :: Rect -> Bool
hasFatAspect r = (min ar (1/ar)) > 0.4
  where ar = aspectRatio r

genStartingRects gen n = let rects = take n $ randoms gen
                         in filter hasFatAspect rects
aspectRatio (Rect w h _) = w/h

main = do
  gen <- getStdGen
  print $ genStartingRects gen 10
