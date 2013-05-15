module DungeonGen where
-- import Debug.Trace
import System.Random
import Data.List (foldl')

import Types


-- an implementation roughly following the tinykeep writeup on reddit/r/gamedev:
-- http://www.reddit.com/r/gamedev/comments/1dlwc4/procedural_dungeon_generation_algorithm_explained/
data Rect = Rect { rW::Int
                 , rH::Int
                 , rPos::Pos
                 } deriving (Show)

toXY :: Float -> Float -> (Float, Float)
toXY angle radius = (radius * sin angle, radius * cos angle)

type Vel = (Float, Float)

data Particle a = Particle {pPos::Pos, pVel:: Vel, pRad :: Float, pContent:: a} deriving (Show)

roomAreaOver ::  Int -> Particle Rect -> Bool
roomAreaOver mn (Particle _ _ _ (Rect w h _p)) = w * h >= mn

isRoom ::  Particle Rect -> Bool
isRoom = roomAreaOver minRoomArea

minRoomArea ::  Int
minRoomArea = 35

rectPos :: Particle Rect -> Pos
rectPos (Particle _ _ _ (Rect _ _ pos)) = pos

rectsToParticles :: [Rect] -> [Particle Rect]
rectsToParticles = map (\rect@(Rect w h pos) -> Particle pos zeroVel (fromIntegral $ max w h) rect)

updateRectWithParticlePosition :: Particle Rect -> Particle Rect
updateRectWithParticlePosition (Particle p v r rect) = (Particle p v r (rect { rPos = p }))

updateVels :: [Particle a] -> [Particle a]
updateVels ps = map (flip updateVelOf ps) ps -- TODO: don't compare to self
-- TODO: add drag

snapParticlePositions ::  Int -> [Particle Rect] -> [Particle Rect]
snapParticlePositions n = map (updateRectWithParticlePosition. snapParticlePosition n)

snapParticlePosition :: Int -> Particle a -> Particle a
snapParticlePosition n (Particle p v r c) = (Particle p' v r c)
  where
    p' = snapPos n p

snapPos :: Int -> Pos -> Pos
snapPos tolerance (x,y) = (f x, f y)
  where 
   f n = t * (fromIntegral ((round (n / t) :: Int)))
   t   = fromIntegral tolerance

addVec :: Pos -> Pos -> Pos
addVec (x,y) (a,b) = (x+a, y+b)

minVel ::  Float
minVel = 0.1

updateParticles ::  [Particle a] -> [Particle a]
updateParticles ps = updatePosns $ updateVels ps

updatePosns ::  [Particle a] -> [Particle a]
updatePosns = map updatePosn

updatePosn ::  Particle a -> Particle a
updatePosn (Particle (x,y) (vx, vy) r c) = (Particle (x+vx, y+vy) (vx,vy) r c)


modVel ::  (Vel -> Vel) -> Particle a -> Particle a
modVel f (Particle p v r c ) = (Particle p (f v)  r c)

applyDrag ::  Particle a -> Particle a
applyDrag p = modVel (f) p
  where f v = if lenVec v < minVel then (0,0) else multVecScalar v 0.4

updateVelOf :: Particle a -> [Particle a] -> Particle a
updateVelOf p@(Particle pos vel rad content) others = applyDrag $ (Particle pos (addVec vel velOffset) rad content)
  where
    velOffset = (`divideVec` (fromIntegral numClosePs)) $ foldl' push (0,0) closePs
      where
        push (vx, vy) pOther = (vx + forceX , vy+forceY)
          where (forceX, forceY) = applyForce (0.3*(distance pos (pPos pOther))) (directionFromTo (pPos pOther) pos)
    closePs    = filter (isClose p) others
    numClosePs = length closePs

applyForce ::  Num t => t -> (t, t) -> (t, t)
applyForce force dir =  multVecScalar dir force

multVecScalar ::  Num t => (t, t) -> t -> (t, t)
multVecScalar (vx,vy) force = (vx * force, vy * force)

lenVec ::  Pos -> Float
lenVec v = distance (0,0) v

normalize :: Pos -> Pos
normalize v = if lenVec v == 0 then v else divideVec v (lenVec v)

directionFromTo ::  Pos -> Pos -> Pos
directionFromTo (x1,y1) (x2,y2) = normalize (x2-x1, y2-y1)

divideVec ::  Fractional t => (t, t) -> t -> (t, t)
divideVec (x,y) d = (x/d, y/d)

negateVec ::  (Num t1, Num t) => (t, t1) -> (t, t1)
negateVec (x,y) = (negate x, negate y)

isClose ::  Particle a -> Particle a1 -> Bool
isClose p1 p2 = dist < ((r1 + r2)/2)
  where
    dist = distance (pPos p1) (pPos p2)
    r1 = pRad p1
    r2 = pRad p2

zeroVel ::  (Float, Float)
zeroVel = (0,0)

distance ::  Pos -> Pos -> Float
distance (x1,y1) (x2,y2) = sqrt (sqr deltaX  + sqr deltaY)
  where deltaX = x1 - x2
        deltaY = y1 - y2
        sqr :: Float -> Float
        sqr x= x^(2::Int)

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
        (radius,g'''')  = randomR (0,50) g'''
        pos     = toXY angle radius

  random = randomR (minRect, maxRect)
    where minRect = Rect 3 3 origin
          maxRect = Rect 10 10 origin
          origin = (0,0)

hasFatAspect :: Rect -> Bool
hasFatAspect r = (min ar (1/ar)) > 0.4
  where ar = aspectRatio r

genStartingRects ::  RandomGen g => g -> Int -> [Rect]
genStartingRects gen n = let rects = take n $ randoms gen
                         in filter hasFatAspect rects
aspectRatio ::  Rect -> Float
aspectRatio (Rect w h _) = fromIntegral w/ fromIntegral h

main ::  IO ()
main = do
  gen <- getStdGen
  print $ genStartingRects gen 10
