{-# LANGUAGE PatternGuards #-}
module Main where
-- import Debug.Trace
import Data.List ((\\))
import Graphics.Gloss.Interface.IO.Game
import System.Environment (getArgs)
import System.Random
import qualified Data.Map as M
import DungeonGen hiding (main)
import Graph 
import Triangulation
import Types

main ::  IO ()
main = do
  putStrLn "press the number keys to snap the box origins to various tolerances"
  gen <- getStdGen
  args <- getArgs
  let display = if elem "-f" args then DMFull else DMWindow
  guimain display gen

data GS = GS { 
    gParticles :: [Particle Rect]
  , gUpdating :: Bool
  , gTolerance :: Int
  , gGridVisible :: Bool
  , gMinArea :: Int
  , gGraphs :: Graphs Pos
  , gVisibilities :: M.Map Int Bool
  , gGen :: StdGen
  } deriving (Show)

data Graphs a = Graphs  { gsBigRoomPs :: [Particle Rect]
                        , gsTriGraph :: Graph a
                        , gsMST :: Graph a
                        , gsMSTWithCycles :: Graph a
                        , gsRectiLines :: [Ln] 
                        , gsIntersectedRoomPs :: [Particle Rect]
                        } 
                        deriving (Show)


reinitGS :: StdGen -> GS -> GS
reinitGS gen gs = GS ps (gUpdating gs) (gTolerance gs) (gGridVisible gs) (gMinArea gs) grs (gVisibilities gs) gen 
  where (ps, grs) = createParticlesAndGraphs gen (gMinArea gs)

initGS ::  StdGen -> GS
initGS gen = GS ps True 9 True minArea grs visbs gen
  where (ps, grs) = createParticlesAndGraphs gen minArea 
        minArea = 40
        visbs = let t = True; f = False in M.fromList $ zip [1..9] [f,t,t,f,t,f,t,f,f]

-- initial reation of random [Particle Rect]. common to init and reinit of the GS
createParticlesAndGraphs :: RandomGen g => g -> Int -> ([Particle Rect], Graphs Pos)
createParticlesAndGraphs gen minArea = (ps, grs)
  where
    rs = genStartingRects gen numStartingRects
    ps = rectsToParticles rs
    grs = computeGraphs gen ps minArea

modMinArea ::  (Int -> Int) -> GS -> GS
modMinArea f gs = gs { gMinArea = f $ gMinArea gs } 
modParticles ::  ([Particle Rect] -> [Particle Rect]) -> GS -> GS
modParticles f gs = gs { gParticles = f (gParticles gs) }

toggleUpdating ::  GS -> GS
toggleUpdating gs = gs { gUpdating = not (gUpdating gs) } 

toggleGridVisibility :: GS -> GS
toggleGridVisibility gs = gs { gGridVisible = not (gGridVisible gs) } 
recomputeGraphs :: GS -> GS
recomputeGraphs gs = gs { gGraphs = computeGraphs gen (gParticles gs) (gMinArea gs) }
  where 
    gen = gGen gs

computeGraphs :: RandomGen g => g -> [Particle Rect] -> Int -> Graphs Pos
computeGraphs gen ps minArea = 
  Graphs bigRoomPs triGraph mst mstWithCycles ls intersectedRoomsPs
  where
    triangles          = triangulate $ map rectPos $ bigRoomPs
    mst                = makeMST triGraph
    triGraph           = trisToGraph triangles
    mstWithCycles      = addSomeEdgesFrom gen (0.10) triGraph mst
    ls                 = genRectiLines mstWithCycles
    bigRoomPs          = filter (roomAreaOver minArea) ps
    intersectedRoomsPs = findIntersectedRooms ls (ps \\ bigRoomPs)

findIntersectedRooms :: [Ln] -> [Particle Rect] -> [Particle Rect]
findIntersectedRooms rectiLines ps = [ p | p<- ps, let (Rect w h _) = pContent p, l <- allSegs rectiLines, intersects p l ]
  where
    intersects particle (p1, p2) = not $ segClearsBox p1 p2 roomLowerLeft roomUpperRight
      where 
        (roomLowerLeft, roomUpperRight) = (roomPos `addVec` (-halfW, -halfH), roomPos `addVec` (halfW, halfH))
        (Rect w h roomPos) = pContent particle
        halfW = fromIntegral w / 2
        halfH = fromIntegral h / 2

    allSegs :: [Ln] -> [(Pos, Pos)]
    allSegs = concatMap segs
    segs points = zip points (drop 1 points)

-- taken from gloss
-- | Check if line segment (P1-P2) clears a box (P3-P4) by being well outside it.
segClearsBox 
  :: Point      -- ^ P1 First point of segment. 
  -> Point      -- ^ P2 Second point of segment.
  -> Point      -- ^ P3 Lower left point of box.
  -> Point      -- ^ P4 Upper right point of box.
  -> Bool

segClearsBox (x1, y1) (x2, y2) (xa, ya) (xb, yb)
  | x1 < xa, x2 < xa    = True
  | x1 > xb, x2 > xb    = True
  | y1 < ya, y2 < ya    = True
  | y1 > yb, y2 > yb    = True
  | otherwise           = False
 

data DisplayMode = DMWindow | DMFull

updateState :: Float -> GS -> IO GS
updateState _f gs = do
  -- Use the stored one, allowing repetition
  -- gen <- newStdGen
  return $ if gUpdating gs
             then gs { gParticles = newParticles
                     , gGraphs = computeGraphs (gGen gs) newParticles (gMinArea gs)
                     }
             else gs
    where 
      newParticles = updateRectsWithParticlePositions $ updateParticles $ gParticles gs

updateRectsWithParticlePositions =  map updateRectWithParticlePosition

numStartingRects ::  Int
numStartingRects = 150

guimain :: DisplayMode -> StdGen  ->  IO ()
guimain dispMode gen = do
  playIO
          (display dispMode)
          white -- background colour
          30 -- number of simulation steps to take for each second of real time
          (initGS gen)
          (return . drawState) -- A function to convert the world into a picture
          (handleInput) -- A function to handle input events
          updateState
  where
    winHeight DMFull = 1280
    winHeight DMWindow = 900
    display DMFull = FullScreen (winHeight DMFull, 800)
    display DMWindow = 
      (InWindow "Dungeon Gen Gloss UI" --name of the window
            (800, winHeight DMWindow) -- initial size of the window
            (0, 0) -- initial position of the window
      )





handleInput :: Event -> GS -> IO GS
handleInput (EventKey (Char 'r') Down _ _) gs            = do
  gen <- newStdGen
  return $ reinitGS gen gs

handleInput (EventKey (Char c) Down mods _) gs | c `elem` "123456789" = 
  if ctrl mods == Down
  then return $ applySnap (read [c]) gs
  else return $ toggleVisibility (read [c]) gs
handleInput (EventKey k                     Down _ _) gs = return $ handleDown k gs
handleInput _                                         gs = return gs -- ignore key ups, and other

handleDown ::  Key -> GS -> GS
handleDown (SpecialKey KeySpace) = toggleUpdating
handleDown (Char       'g')      = toggleGridVisibility
handleDown (SpecialKey KeyDown)  = modMinArea (+3)
handleDown (SpecialKey KeyUp)    = modMinArea (subtract 3)
handleDown (SpecialKey KeyLeft)  = id
handleDown (SpecialKey KeyRight) = id
handleDown (MouseButton LeftButton) = id
handleDown _ = id
toggleVisibility :: Int -> GS -> GS
toggleVisibility i gs = gs { gVisibilities = M.adjust not i (gVisibilities gs) }
applySnap :: Int -> GS -> GS
applySnap i = stopUpdates . recomputeGraphs . modParticles updateRectsWithParticlePositions . setTolerance i . modParticles (snapParticlePositions (2*i))

setTolerance ::  Int -> GS -> GS
setTolerance i gs = gs { gTolerance = i }

stopUpdates ::  GS -> GS
stopUpdates gs = gs { gUpdating = False } 

drawGrid :: Int -> Picture
drawGrid tol = Color (dark $ dark $ dark$dark$ green) $ Pictures  $
  [ Line [(-4000, y), (4000, y)] | i<- [-50..50], let y = fromIntegral (i*tol)] ++ 
  [ Line [(x, -4000), (x, 4000)] | i<- [-50..50], let x = fromIntegral $ i*tol] 


drawAxes :: Picture
drawAxes = Color yellow $ Pictures [
    Line [(0,-4000), (0,4000)]
  , Line [(-4000,0), (4000,0)]
  ]

drawState :: GS -> Picture
drawState gs = Pictures $ [ 
    drawBackground
  , scale 5 5 $ Pictures [ 
      if gGridVisible gs then drawGrid (gTolerance gs) else blank
      -- , drawAxes
     , translate (0) (0) $ onlyOn 1 $ Color green $ Pictures $ map drawParticle (gParticles gs)
     , translate (0) (0) $ onlyOn 2 $ Color (dark $ dark blue) $ Pictures $ map drawParticle (gsIntersectedRoomPs grs) 
     , translate (0) (0) $ onlyOn 3 $ Color green $ Pictures $ map drawParticle (gsBigRoomPs grs) 
     , translate (0) (0) $ onlyOn 4 $ Color (dark $ dark yellow) $ drawGraph $ gsTriGraph grs
     , translate (0) (0) $ onlyOn 5 $ Color red $ drawGraph (gsMSTWithCycles grs)
     , translate (0) (0) $ onlyOn 6 $ Color white $ drawGraph (gsMST grs)
     , translate (0) (0) $ onlyOn 7 $ Color yellow $ drawLines (gsRectiLines grs)
    ]
  ]
  where grs = gGraphs gs 
        onlyOn n pic = if gVisibilities gs M.! n then pic else Blank


drawGraph :: Graph Pos -> Picture
drawGraph (Graph ns es) = Pictures [Pictures $ map drawNode ns, Pictures $ map drawEdge es]
  where
    drawNode (x,y) = translate x y $ rectangleSolid 1 1
    drawEdge (Edge (p1,p2)) = Line [p1,p2]
drawTris :: [Triangle] -> Picture
drawTris = Pictures . map drawTri
drawTri ::  (Point, Point, Point) -> Picture
drawTri (p1,p2,p3) = Line [p1,p2,p3]
drawLines = Pictures . map drawLine
drawLine ps = Line ps

drawParticle ::  Particle Rect -> Picture
drawParticle (Particle (x,y) _vel _rad (Rect w h _)) = translate x y $ rectangleWire wf hf
  where wf = fromIntegral w
        hf = fromIntegral h

drawBackground :: Picture
drawBackground = Color black $ rectangleSolid 2000 2000 

