module Main where
import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)
import Debug.Trace
import Graphics.Gloss.Interface.IO.Game
import System.Environment (getArgs)
import DungeonGen hiding (main)
import System.Random

main ::  IO ()
main = do
  gen <- getStdGen
  args <- getArgs
  let display = if elem "-f" args then DMFull else DMWindow
  guimain display gen

data GS = GS { 
    msgs :: [Float] 
  , particles :: [Particle Rect]
  } deriving (Show)

initGS gen = GS [] ps
  where rs = genStartingRects gen numStartingRects
        ps = rectsToParticles rs


data DisplayMode = DMWindow | DMFull

updateState :: Float -> GS -> GS
updateState f gs = gs { particles = updateParticles $ particles gs }

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
handleInput (EventKey (SpecialKey KeySpace) Down _ _) gs = putStrLn "hello" >> return gs
handleInput (EventKey k                     Down _ _) gs = return $ handleDown k gs
handleInput _                                         gs = return gs -- ignore key ups, and other

modParticles f gs = gs { particles = f (particles gs) }
handleDown ::  Key -> GS -> GS
handleDown (Char       'r')      = id
handleDown (SpecialKey KeyDown)  = id
handleDown (SpecialKey KeyUp)    = id
handleDown (SpecialKey KeyLeft)  = id
handleDown (SpecialKey KeyRight) = id
handleDown (Char       c) | c `elem` "123456789" = modParticles (snapParticlePositions (2*read [c]))
                          | otherwise            = id
handleDown (MouseButton LeftButton) = id
handleDown _ = id

drawAxes :: Picture
drawAxes = Color yellow $ Pictures [
    Line [(0,-4000), (0,4000)]
  , Line [(-4000,0), (4000,0)]
  ]

drawState :: GS -> Picture
drawState gs = Pictures $ [ 
    drawBackground
  , drawAxes
  , translate (0) (0) $ Color green $ Pictures $ map drawParticle (particles gs)
  ]

drawParticle (Particle (x,y) (vx,vy) rad (Rect w h _)) = translate x y $ rectangleWire w h
drawBackground :: Picture
drawBackground = Color black $ rectangleSolid 2000 2000 

