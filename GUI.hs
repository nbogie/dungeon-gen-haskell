module Main where
import Debug.Trace
import Graphics.Gloss.Interface.IO.Game
import System.Environment (getArgs)
import DungeonGen hiding (main)
import System.Random

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
  } deriving (Show)

modParticles ::  ([Particle Rect] -> [Particle Rect]) -> GS -> GS
modParticles f gs = gs { particles = f (particles gs) }

toggleUpdating ::  GS -> GS
toggleUpdating gs = gs { updating = not (updating gs) } 
toggleGridVisibility :: GS -> GS
toggleGridVisibility gs = gs { gridVisible = not (gridVisible gs) } 

initGS ::  RandomGen g => g -> GS
initGS gen = GS [] ps True 1 True
  where rs = genStartingRects gen numStartingRects
        ps = rectsToParticles rs


data DisplayMode = DMWindow | DMFull

updateState :: Float -> GS -> GS
updateState _f gs = if updating gs
  then gs { particles = updateParticles $ particles gs }
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
handleInput (EventKey k                     Down _ _) gs = return $ handleDown k gs
handleInput _                                         gs = return gs -- ignore key ups, and other

handleDown ::  Key -> GS -> GS
handleDown (SpecialKey KeySpace) = toggleUpdating
handleDown (Char       'r')      = id
handleDown (Char       'g')      = toggleGridVisibility
handleDown (SpecialKey KeyDown)  = id
handleDown (SpecialKey KeyUp)    = id
handleDown (SpecialKey KeyLeft)  = id
handleDown (SpecialKey KeyRight) = id
handleDown (Char       c) | c `elem` "123456789" = applySnap (read [c])
                          | otherwise            = id
handleDown (MouseButton LeftButton) = id
handleDown _ = id
applySnap i = stopUpdates . setTolerance i . modParticles (snapParticlePositions (2*i))
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
    ]
  ]

drawParticle ::  Particle Rect -> Picture
drawParticle (Particle (x,y) (vx,vy) rad (Rect w h _)) = translate x y $ rectangleWire wf hf
  where wf = fromIntegral w
        hf = fromIntegral h

drawBackground :: Picture
drawBackground = Color black $ rectangleSolid 2000 2000 

