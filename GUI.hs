module Main where
-- import Debug.Trace
import Graphics.Gloss.Interface.IO.Game
import System.Environment (getArgs)
import DungeonGen hiding (main)
import Triangulation
import System.Random
import Graph 
import Types

main ::  IO ()
main = do
  putStrLn "press the number keys to snap the box origins to various tolerances"
  gen <- getStdGen
  args <- getArgs
  let display = if elem "-f" args then DMFull else DMWindow
  guimain display gen

data GS = GS { 
    msgs :: [Float] 
  , particles :: [Particle Rect]
  , updating :: Bool
  , tolerance :: Int
  , gridVisible :: Bool
  , minArea :: Int
  , graph :: Maybe ( Graph Pos)
  } deriving (Show)

modMinArea ::  (Int -> Int) -> GS -> GS
modMinArea f gs = gs { minArea = f $ minArea gs } 
modParticles ::  ([Particle Rect] -> [Particle Rect]) -> GS -> GS
modParticles f gs = gs { particles = f (particles gs) }

toggleUpdating ::  GS -> GS
toggleUpdating gs = gs { updating = not (updating gs) } 
toggleGridVisibility :: GS -> GS
toggleGridVisibility gs = gs { gridVisible = not (gridVisible gs) } 

initGS ::  RandomGen g => g -> GS
initGS gen = GS [] ps True 9 True 40 gr
  where rs = genStartingRects gen numStartingRects
        ps = rectsToParticles rs
        gr = Nothing


data DisplayMode = DMWindow | DMFull

updateState :: Float -> GS -> GS
updateState _f gs = if updating gs
  then gs { particles = map updateRectWithParticlePosition $ updateParticles $ particles gs }
  else gs


numStartingRects ::  Int
numStartingRects = 150

guimain :: (RandomGen g) => DisplayMode -> g ->  IO ()
guimain dispMode gen = do
  playIO
          (display dispMode)
          white -- background colour
          30 -- number of simulation steps to take for each second of real time
          (initGS gen)
          (return . drawState) -- A function to convert the world into a picture
          (handleInput) -- A function to handle input events
          (\f g -> return $ updateState f g)
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
handleInput (EventKey (Char 'r') Down _ _) _gs           = fmap initGS newStdGen
handleInput (EventKey k                     Down _ _) gs = return $ handleDown k gs
handleInput _                                         gs = return gs -- ignore key ups, and other

handleDown ::  Key -> GS -> GS
handleDown (SpecialKey KeySpace) = toggleUpdating
handleDown (Char       'r')      = id
handleDown (Char       'g')      = toggleGridVisibility
handleDown (SpecialKey KeyDown)  = modMinArea (+3)
handleDown (SpecialKey KeyUp)    = modMinArea (subtract 3)
handleDown (SpecialKey KeyLeft)  = id
handleDown (SpecialKey KeyRight) = id
handleDown (Char       c) | c `elem` "123456789" = applySnap (read [c])
                          | otherwise            = id
handleDown (MouseButton LeftButton) = id
handleDown _ = id
applySnap ::  Int -> GS -> GS
applySnap i = stopUpdates . setTolerance i . modParticles (snapParticlePositions (2*i))
setTolerance ::  Int -> GS -> GS
setTolerance i gs = gs { tolerance = i }
stopUpdates ::  GS -> GS
stopUpdates gs = gs { updating = False } 

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
      if gridVisible gs then drawGrid (tolerance gs) else blank
      -- , drawAxes
     , translate (0) (0) $ Color green $ Pictures $ map drawParticle (particles gs)
     -- , translate (0) (0) $ Color (dark $ dark yellow) $ drawTris $ triangles 
     , translate (0) (0) $ Color white $ drawGraph mst
    ]
  ]
  where 
    triangles = triangulate $ map rectPos $ filter (roomAreaOver (minArea gs)) (particles gs)
    mst  = makeMST $ trisToGraph triangles

drawGraph :: Graph Pos -> Picture
drawGraph (Graph ns es) = Pictures [Pictures $ map drawNode ns, Pictures $ map drawEdge es]
  where
    drawNode (x,y) = translate x y $ rectangleSolid 1 1
    drawEdge (Edge (p1,p2)) = Line [p1,p2]
drawTris :: [Triangle] -> Picture
drawTris = Pictures . map drawTri
drawTri ::  (Point, Point, Point) -> Picture
drawTri (p1,p2,p3) = Line [p1,p2,p3]

drawParticle ::  Particle Rect -> Picture
drawParticle (Particle (x,y) _vel _rad (Rect w h _)) = translate x y $ rectangleWire wf hf
  where wf = fromIntegral w
        hf = fromIntegral h

drawBackground :: Picture
drawBackground = Color black $ rectangleSolid 2000 2000 

