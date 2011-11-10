import SimpleDrawing
import Control.Concurrent
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.State.Strict

mvar :: MVar Cmd
mvar = unsafePerformIO newEmptyMVar

mvarIO :: MVar (IO ())
mvarIO = unsafePerformIO newEmptyMVar

type Distance = Double
type Angle    = Double
type MyColor  = (GLfloat, GLfloat, GLfloat)
data Cmd = Forward
         | Turn Angle
         | CStep Double
         | PenUp
         | PenDown
         | Push
         | Pop
         | DipPen MyColor
         | Clean
         | Reset
         deriving (Show)

data Turtle = Turtle { position :: Point
                     , step     :: Distance
                     , angle    :: Angle
                     , pen      :: Bool 
                     , color    :: MyColor } deriving (Show)

turtle0 = Turtle { position = (0,0)
                 , angle    = 0
                 , step     = 0.1
                 , pen      = True 
                 , color    = (1,1,1)}

process = evalStateT program (turtle0, [])
          
type StateTurtle a = StateT (Turtle,[Turtle]) (IO) a

program :: StateTurtle ()
program = do
  cmd <- lift $ takeMVar mvar
  turtle <- getTurtle
  case cmd of
    PenUp -> putTurtle (turtle { pen = False })
    PenDown -> putTurtle (turtle { pen = True })
    DipPen (r,g,b) -> putTurtle (turtle {color = (r,g,b)})
    Forward -> do
      let (x0,y0) = position turtle
          a = angle turtle
          d = step turtle
          (r,g,b) = color turtle
          (dx,dy) = fromPolar2D (d,a)
          (x1,y1) = (x0 + dx, y0 + dy)
          turtle' = turtle { position = (x1,y1) }
      putTurtle turtle'
      case pen turtle of
        True -> lift $ putMVar mvarIO $ do
          currentColor $= Color4 r g b 0
          draw $ Line (x0,y0) (x1,y1)
          flush
        False -> return ()
    Turn a -> putTurtle $ turtle { angle = angle turtle + (pi*a/180) }
    Push -> push
    Pop  -> pop
    CStep s -> do
      putTurtle $ turtle { step = step turtle * s }
    Clean -> lift $ putMVar mvarIO clear
    Reset -> put $ (turtle0, [])
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

example1 = replicateM_ 12 (p >> turn 30) where
  p = replicateM_ 12 (forward >> turn 30)
  
example2 = cstep 2 >> tree1 12

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
  run2 mvarIO
  forkIO process
  forkIO mainLoop
  
send :: Cmd -> IO ()
send = putMVar mvar

cstep s = send $ CStep s
forward = send Forward
turn a  = send $ Turn a
pop     = send Pop
push    = send Push
reset   = send Reset
clean   = send Clean
repl n  = replicateM_ n
penup   = send PenUp
pendown = send PenDown
dippen  = send . DipPen

example3 = mapM_ (\c -> h c fig) $ concat (repeat cs)
  where
    fig = cstep 0.2 >> replicateM_ 5 (replicateM_ 6 (forward >> turn 60) >> turn 72)
    h c fig = penup >> dippen c >> push >> pendown >> fig >> pop >> forward >> turn 16 >> cstep 0.99
    cs = zipWith (\a b -> (a,b,0)) xs' ys'
    xs' = xs ++ replicate 10 1.0
    ys' = replicate 10 1.0 ++ reverse xs
    xs  = [0,0.1..1.0]