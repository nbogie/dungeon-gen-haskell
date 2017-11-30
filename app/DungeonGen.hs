{-# LANGUAGE PatternGuards #-}
module DungeonGen where
-- import Debug.Trace
import System.Random
import DataRandomNormal
import Data.List (foldl')
import qualified Data.Map as M

import Types
import Graph

-- an implementation roughly following the tinykeep writeup on reddit/r/gamedev:
-- http://www.reddit.com/r/gamedev/comments/1dlwc4/procedural_dungeon_generation_algorithm_explained/
data Rect = Rect { 
    rW   :: Int
  , rH   :: Int
  , rPos :: Pos
  } deriving (Show, Eq, Read)

angleRadToXY :: Float -> Float -> (Float, Float)
angleRadToXY angle radius = (radius * sin angle, radius * cos angle)

type Vel = (Float, Float)

occupied :: [Rect] -> Pos -> Bool
occupied rs p = any (occupiedBy p) rs

occupiedBy ::  (Float, Float) -> Rect -> Bool
occupiedBy (x,y) (Rect w h (rx,ry)) = x `within` (rx `extendedTo` fi w) &&
                                      y `within` (ry `extendedTo` fi h)
  where
    fi                 = fromIntegral
    extendedTo v range = (v-halfR, v+halfR) where halfR = range / 2
    within v (mn,mx)   = v > mn && v < mx

data Particle a = Particle { 
    pPos    :: Pos
  , pVel    :: Vel
  , pRad    :: Float
  , pContent:: a
  } deriving (Show, Eq, Read)

roomAreaOver ::  Int -> Particle Rect -> Bool
roomAreaOver mn p = rectArea (pContent p)  >= mn

rectArea ::  Rect -> Int
rectArea (Rect w h _p) = w * h

isRoom ::  Particle Rect -> Bool
isRoom = roomAreaOver minRoomArea

minRoomArea :: Int
minRoomArea = 40

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
  -- TODO: generate within given range, or don't use an instance of Random
  -- (This design doesn't fit well with non-uniform distributions)
  randomR _lims gen = 
    if hasFatAspect r then (r,g'''') else randomR _lims g''''
      where 
        r       = Rect w h pos
        (w,g')  = randomDim gen
        (h,g'') = randomDim g'

        angle, radius :: Float
        (angle,g''')   = randomR (0,359) g''
        (radius,g'''') = randomR (0,50) g'''
        pos            = angleRadToXY angle radius

  random = randomR (minRect, maxRect)
    where minRect = Rect 1 1 origin
          maxRect = Rect 5 5 origin
          origin = (0,0)

randomDim :: (RandomGen g) => g -> (Int, g)
randomDim g = (d, g')
  where 
      d      = 2 * round (2+abs n) - 1
      (n,g') = normal' (1::Float, 2::Float) g

hasFatAspect :: Rect -> Bool
hasFatAspect r = (min ar (1/ar)) > 0.4
  where ar = aspectRatio r

genStartingRects ::  RandomGen g => g -> Int -> [Rect]
genStartingRects gen n = let rects = take n $ randoms gen
                         in filter hasFatAspect rects

aspectRatio ::  Rect -> Float
aspectRatio (Rect w h _) = fromIntegral w/ fromIntegral h

pickOne :: (RandomGen g) => [a] -> g -> (a, g)
pickOne xs gen = (xs !! i, g')
  where (i, g') = randomR (0, length xs - 1) gen

centreBounds :: [Pos] -> Maybe (Pos, Pos)
centreBounds [] = Nothing
centreBounds ps = Just ((leftX, bottomY), (rightX, topY))
  where 
    topY    = maximum ys
    bottomY = minimum ys
    leftX   = minimum xs
    rightX  = maximum xs
    xs      = map fst ps
    ys      = map snd ps

generateFillerFor :: (Pos, Pos) -> [Particle Rect] -> [Rect]
generateFillerFor ((x1,y1), (x2,y2)) ps = newRects
  where 
    newRects = [Rect 1 1 (x,y) | x <- [x1..x2], y<-[y1..y2]
                               , (not . occupied currRects) (x,y)]
    currRects = map pContent ps

findIntersectedRooms :: [Ln] -> [Particle Rect] -> [Particle Rect]
findIntersectedRooms rectiLines ps = [ p | p<- ps, l <- allSegs rectiLines, intersects p l ]
  where
    intersects particle (p1, p2) = not $ segClearsBox p1 p2 roomLowerLeft roomUpperRight
      where 
        (roomLowerLeft, roomUpperRight) = (roomPos `addVec` (-halfW, -halfH), 
                                           roomPos `addVec` (halfW, halfH))
        (Rect w h roomPos) = pContent particle
        halfW = fromIntegral w / 2
        halfH = fromIntegral h / 2

    allSegs :: [Ln] -> [(Pos, Pos)]
    allSegs = concatMap segs
    segs points = zip points (drop 1 points)


-- taken straight from gloss.  license?
-- | Check if line segment (P1-P2) clears a box (P3-P4) by being well outside it.
segClearsBox 
  :: Pos      -- ^ P1 First point of segment. 
  -> Pos      -- ^ P2 Second point of segment.
  -> Pos      -- ^ P3 Lower left point of box.
  -> Pos      -- ^ P4 Upper right point of box.
  -> Bool

segClearsBox (x1, y1) (x2, y2) (xa, ya) (xb, yb)
  | x1 < xa, x2 < xa    = True
  | x1 > xb, x2 > xb    = True
  | y1 < ya, y2 < ya    = True
  | y1 > yb, y2 > yb    = True
  | otherwise           = False

main :: IO ()
main = do
  gen <- getStdGen
  let rs = genStartingRects gen 100
  print $ (histOf rW rs :: M.Map Int Int)
  print $ (histOf rH rs :: M.Map Int Int)
  print $ (histOf rectArea rs :: M.Map Int Int)

histOf ::  (Num a, Ord b) => (a1 -> b) -> [a1] -> M.Map b a
histOf f = hist . map f 

hist ::  (Num a, Ord b) => [b] -> M.Map b a
hist ns = foldl' f M.empty ns
  where
    f m k = M.insertWith (+) k 1 m

