module Main where
-- import Debug.Trace
import Data.Maybe (fromMaybe)
import Data.List ((\\))
import qualified Data.Map as M
import Graphics.Gloss.Interface.IO.Game
import System.Environment (getArgs)
import System.Random
import Text.Show.Pretty (ppShow)

import DungeonGen hiding (main)
import Graph
import Triangulation(triangulate,Triangle)
import Types


main ::  IO ()
main = do
  putStrLn "press ctrl-numbers to snap the box origins to various tolerances"
  gen  <- getStdGen
  args <- getArgs
  let display = if elem "-f" args then DMFull else DMWindow
  guimain display gen

data GS = GS { 
    gParticles    :: [Particle Rect]
  , gUpdating     :: Bool
  , gTolerance    :: Int
  , gGridVisible  :: Bool
  , gMinArea      :: Int
  , gCyclesPct    :: Float
  , gGraphs       :: Graphs Pos
  , gVisibilities :: M.Map Int Bool
  , gGen          :: StdGen
  } deriving (Show)

data Graphs a = Graphs  { 
    gsBigRoomPs           :: [Particle Rect]
  , gsTriGraph            :: Graph a
  , gsMST                 :: Graph a
  , gsMSTWithCycles       :: Graph a
  , gsRectiLines          :: [Ln] 
  , gsIntersectedRoomPs   :: [Particle Rect]
  , gsFillerPs            :: [Particle Rect]
  , gsIntersectedFillerPs :: [Particle Rect]
  } deriving (Show)


reinitGS :: StdGen -> GS -> GS
reinitGS gen gs = GS ps (gUpdating gs) (gTolerance gs) (gGridVisible gs)
                     (gMinArea gs) (gCyclesPct gs) grs (gVisibilities gs) gen 
  where (ps, grs) = createParticlesAndGraphs gen (gMinArea gs) (gCyclesPct gs)

initGS ::  StdGen -> GS
initGS gen = GS ps True 9 True minArea cyclesPct grs visbs gen
  where 
        (ps, grs) = createParticlesAndGraphs gen minArea cyclesPct
        minArea   = 60
        cyclesPct = 0.10
        visbs     = M.fromList $ zip [1..9] [t,t,t,f,f,f,f,t,f] where t=True; f=False

-- initial reation of random [Particle Rect]. common to init and reinit of the GS
createParticlesAndGraphs :: RandomGen g => g -> Int -> Float -> ([Particle Rect], Graphs Pos)
createParticlesAndGraphs gen minArea cyclesPct = (ps, grs)
  where
    rs = genStartingRects gen numStartingRects
    ps = rectsToParticles rs
    grs = computeGraphs gen ps minArea cyclesPct

modMinArea ::  (Int -> Int) -> GS -> GS
modMinArea f gs = recomputeGraphs $ gs { gMinArea = f $ gMinArea gs } 

modCyclesPct ::  (Float -> Float) -> GS -> GS
modCyclesPct f gs = recomputeGraphs $ gs'
  where 
    gs' = gs { gCyclesPct = limit (0,1) $ f $ gCyclesPct gs } 
    limit (mn,mx) v | v < mn    = mn
                    | v > mx    = mx
                    | otherwise = v

modParticles ::  ([Particle Rect] -> [Particle Rect]) -> GS -> GS
modParticles f gs = gs { gParticles = f (gParticles gs) }

toggleUpdating ::  GS -> GS
toggleUpdating gs = gs { gUpdating = not (gUpdating gs) } 

toggleGridVisibility :: GS -> GS
toggleGridVisibility gs = gs { gGridVisible = not (gGridVisible gs) } 

recomputeGraphs :: GS -> GS
recomputeGraphs gs = gs { gGraphs = computeGraphs gen (gParticles gs) (gMinArea gs) (gCyclesPct gs)}
  where 
    gen = gGen gs

computeGraphs :: RandomGen g => g -> [Particle Rect] -> Int -> Float -> Graphs Pos
computeGraphs gen ps minArea cyclesPct = 
  Graphs bigRoomPs triGraph mst mstWithCycles ls intersectedRoomsPs fillerPs intersectedFillerPs
  where
    triangles           = triangulate $ map rectPos $ bigRoomPs
    mst                 = makeMST triGraph
    triGraph            = trisToGraph triangles
    mstWithCycles       = addSomeEdgesFrom gen cyclesPct triGraph mst
    ls                  = genRectiLines mstWithCycles
    bigRoomPs           = filter (roomAreaOver minArea) ps
    intersectedRoomsPs  = findIntersectedRooms ls (ps \\ bigRoomPs)
    boundsM             = centreBounds $ map (rPos. pContent) bigRoomPs
    fillerPs            = rectsToParticles $ generateFillerFor (fromMaybe defaultFillerBounds boundsM) ps
    intersectedFillerPs = findIntersectedRooms ls fillerPs

-- how much to fill in if there are no major rooms
defaultFillerBounds :: (Pos, Pos)
defaultFillerBounds = ((-50,-50), (50, 50))

 

data DisplayMode = DMWindow | DMFull

updateState :: Float -> GS -> IO GS
updateState _f gs = do
  -- Use the stored one, allowing repetition
  -- gen <- newStdGen
  return $ if gUpdating gs
             then gs { gParticles = newParticles
                     , gGraphs = computeGraphs (gGen gs) newParticles (gMinArea gs) (gCyclesPct gs)
                     }
             else gs
    where 
      newParticles = updateRectsWithParticlePositions $ updateParticles $ gParticles gs

updateRectsWithParticlePositions :: [Particle Rect] -> [Particle Rect]
updateRectsWithParticlePositions =  map updateRectWithParticlePosition

numStartingRects ::  Int
numStartingRects = 150

guimain :: DisplayMode -> StdGen  ->  IO ()
guimain dispMode gen = do
  playIO
          (display dispMode)
          (black)              -- background colour
          30                   -- number of simulation steps to take for each second of real time
          (initGS gen)
          (return . drawState) -- A function to convert the world into a picture
          (handleInput)        -- A function to handle input events
          updateState
  where
    winHeight DMFull = 1280
    winHeight DMWindow = 900
    display DMFull = FullScreen (winHeight DMFull, 800)
    display DMWindow = 
      (InWindow "Dungeon Gen Gloss UI" -- name of the window
            (800, winHeight DMWindow)  -- initial size of the window
            (0, 0)                     -- initial position of the window
      )





handleInput :: Event -> GS -> IO GS
handleInput (EventKey (Char 'r') Down _ _) gs    = do
  gen <- newStdGen
  return $ reinitGS gen gs
handleInput (EventKey (Char 'd') Down _ _) gs    = do
  _ <- writeFile "dump.txt" (ppShow (gGraphs gs))
  return gs

handleInput (EventKey (Char c) Down mods _) gs | c `elem` "123456789" = 
  if ctrl mods == Down
  then return $ applySnap (read [c]) gs
  else return $ toggleVisibility (read [c]) gs
handleInput (EventKey k                     Down _ _) gs = return $ handleDown k gs
handleInput _                                         gs = return gs -- ignore key ups, and other

handleDown ::  Key -> GS -> GS
handleDown (SpecialKey KeySpace)    = toggleUpdating
handleDown (Char       'g')         = toggleGridVisibility
handleDown (SpecialKey KeyDown)     = modMinArea (+3)
handleDown (SpecialKey KeyUp)       = modMinArea (subtract 3)
handleDown (SpecialKey KeyLeft)     = modCyclesPct (subtract 0.01)
handleDown (SpecialKey KeyRight)    = modCyclesPct (+0.01)
handleDown (MouseButton LeftButton) = id
handleDown _                        = id

toggleVisibility :: Int -> GS -> GS
toggleVisibility i gs = gs { gVisibilities = M.adjust not i (gVisibilities gs) }

applySnap :: Int -> GS -> GS
applySnap i = stopUpdates . 
              recomputeGraphs .
              modParticles updateRectsWithParticlePositions .
              setTolerance i .
              modParticles (snapParticlePositions (2*i))

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

drawBounds :: [Particle Rect] -> Picture
drawBounds ps = translate x y $ rectangleWire w h
  where 
    (w,h,(x,y)) = boundsToGlossRect (fromMaybe defaultFillerBounds boundsM)
    boundsM     = centreBounds $ map (rPos . pContent) ps

boundsToGlossRect :: (Fractional t, Fractional t1) =>((t, t1), (t, t1)) -> (t, t1, (t, t1))
boundsToGlossRect ((x1,y1), (x2,y2)) = (w,h,(x0,y0))
  where x0 = x1 + (w/2)
        y0 = y1 + (h/2)
        w = (x2 - x1)
        h = (y2 - y1)

data FillMode = Wire | Solid deriving (Eq)
drawState :: GS -> Picture
drawState gs = 
  scale 6 6 $ Pictures [ 
      if gGridVisible gs then drawGrid (gTolerance gs) else blank
      -- , drawAxes
     , onlyOn 1 $ Color ddddgreen $ drawParticles Wire (gParticles gs)
     , onlyOn 2 $ Color ddgreen   $ drawParticles Wire (gsIntersectedRoomPs grs) 
     , onlyOn 7 $ Color yellow    $ drawLines $ gsRectiLines grs
     , onlyOn 3 $ Color green     $ drawParticles Solid (gsBigRoomPs grs) 
     , onlyOn 4 $ Color ddyellow  $ drawGraph $ gsTriGraph grs
     , onlyOn 5 $ Color red       $ drawGraph $ gsMSTWithCycles grs
     , onlyOn 6 $ Color white     $ drawGraph $ gsMST grs
     , onlyOn 9 $ Color orange    $ drawParticles Wire $ gsFillerPs grs
     , onlyOn 8 $ Color white     $ drawParticles Wire $ gsIntersectedFillerPs grs
    ]
  where grs = gGraphs gs 
        onlyOn n pic = if gVisibilities gs M.! n then pic else Blank

        dddddgreen = dark . dark . dark . dark . dark $ green
        ddddgreen  = dark . dark . dark . dark $ green
        dddgreen   = dark . dark . dark $ green
        ddgreen    = dark . dark $ green
        dgreen     = dark  green
        lgreen     = light $ green
        ddyellow   = dark . dark $ yellow
        ddblue     = dark . dark $ blue
        dddblue    = dark . dark . dark $ blue


drawGraph :: Graph Pos -> Picture
drawGraph (Graph ns es) = Pictures [Pictures $ map drawNode ns, Pictures $ map drawEdge es]
  where
    drawNode (x,y) = translate x y $ rectangleSolid 1 1
    drawEdge (Edge (p1,p2)) = Line [p1,p2]

drawTris :: [Triangle] -> Picture
drawTris = Pictures . map drawTri

drawTri ::  (Point, Point, Point) -> Picture
drawTri (p1,p2,p3) = Line [p1,p2,p3]

drawLines ::  [Path] -> Picture
drawLines = Pictures . map drawLine
drawLine ::  Path -> Picture
drawLine ps = Line ps

drawParticles :: FillMode -> [Particle Rect] -> Picture
drawParticles fillMode = Pictures . map (drawParticle fillMode)

drawParticle :: FillMode -> Particle Rect -> Picture
drawParticle fillMode (Particle (x,y) _vel _rad (Rect w h _)) = 
  translate x y $ geomFn wf hf
  where 
        geomFn = case fillMode of; Solid -> rectangleSolid; Wire -> rectangleWire
        wf = fromIntegral w
        hf = fromIntegral h

