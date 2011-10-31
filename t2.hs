import SimpleDrawing
import Control.Concurrent
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.State.Strict

mvar :: MVar Cmd
mvar = unsafePerformIO newEmptyMVar

type Distance = Double
type Angle    = Double

data Cmd = Forward
         | Turn Angle
         | CStep Double
         | PenUp
         | PenDown
         | Push
         | Pop
         deriving (Show)

data Turtle = Turtle { position :: Point
                     , step     :: Distance
                     , angle    :: Angle
                     , pen      :: Bool } deriving (Show)

process = run1 $ evalStateT program (Turtle { position = (0,0)
                                            , angle    = 0 
                                            , step     = 0.1
                                            , pen      = True }, [])
          
type StateTurtle a = StateT (Turtle,[Turtle]) (IO) a


program :: StateTurtle ()
program = do
  cmd <- lift $ takeMVar mvar
  turtle <- getTurtle
  case cmd of
    PenUp -> putTurtle (turtle { pen = False })
    PenDown -> putTurtle (turtle { pen = True })
    Forward -> do
      let (x0,y0) = position turtle
          a = angle turtle
          d = step turtle
          (dx,dy) = fromPolar2D (d,a)
          (x1,y1) = (x0 + dx, y0 + dy)
          turtle' = turtle { position = (x1,y1) }
      putTurtle turtle'
      case pen turtle of
        True -> do
          lift $ draw $ Line (x0,y0) (x1,y1)
          lift $ flush
        False -> return ()
    Turn a -> putTurtle $ turtle { angle = angle turtle + (pi*a/180) }
    Push -> push
    Pop  -> pop
    CStep s -> do
      putTurtle $ turtle { step = step turtle * s }
  program   
  where
    getTurtle = fst `liftM` get
    putTurtle t = do
      (_,s) <- get
      put (t,s)
  
    pop = do  
      (x,s) <- get
      case null s of
        True  -> return ()
        False -> put (head s, tail s) 

    push = do
      (x,s) <- get
      put (x,x:s)
    
tree1 = tree (30,0.8) (20,0.85)

tree (a0,s0) (a1,s1) n | n == 0 = return ()
                       | otherwise = do
  forward
  push
  turn (-a0)
  cstep s0
  tree (a0,s0) (a1,s1) (n-1)
  pop 
  push
  turn a1
  cstep s1
  tree (a0,s0) (a1,s1) (n-1)
  pop

main = do
  process
  forkIO mainLoop
  
send :: Cmd -> IO ()
send = putMVar mvar

cstep s = send $ CStep s
forward = send Forward
turn a = send $ Turn a
pop = send Pop
push = send Push