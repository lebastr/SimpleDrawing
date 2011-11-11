module Turtle ( Turtle (..)
              , penup
              , pendown
              , dippen
              , forward
              , turn
              , cstep
              , clean
              , reset
              , pop
              , push
              , send
              , getTurtle 
              , mvarIO 
              , turtleProcess 
              , StateTurtle 
              , MyColor 
              , Distance
              , Angle 
              , setpos
              , setangle ) where

import SimpleDrawing
import Control.Concurrent
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.State.Strict

mvar :: MVar (StateTurtle ())
mvar = unsafePerformIO newEmptyMVar

mvarIO :: MVar (IO ())
mvarIO = unsafePerformIO newEmptyMVar

type Distance = Radius
-- type Angle    = Double
type MyColor  = (GLfloat, GLfloat, GLfloat)

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
          
type StateTurtle = StateT (Turtle,[Turtle]) IO

setpos (x,y) = do
  t <- getTurtle
  putTurtle (t {position = (x,y)})
  
setangle a = do
  t <- getTurtle
  putTurtle (t {angle = a})

penup :: StateT (Turtle, [Turtle]) IO ()
penup = do
  t <- getTurtle
  putTurtle (t {pen = False})
  
pendown :: StateT (Turtle, [Turtle]) IO ()
pendown = do
  t <- getTurtle
  putTurtle (t {pen = True})
  
dippen :: MyColor -> StateT (Turtle, [Turtle]) IO ()
dippen c = do
  t <- getTurtle
  putTurtle (t {color = c})

forward :: StateT (Turtle, [Turtle]) IO ()
forward = do
  turtle <- getTurtle
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
  
turn :: Angle -> StateT (Turtle, [Turtle]) IO ()
turn a = do
  t <- getTurtle
  putTurtle $ t { angle = angle t + (pi*a/180) }

cstep :: Distance -> StateT (Turtle, [Turtle]) IO ()
cstep k = do
  t <- getTurtle
  putTurtle $ t { step = k * step t }

clean :: StateTurtle ()
clean = lift $ putMVar mvarIO clear

reset :: StateTurtle ()
reset = put $ (turtle0, [])

getTurtle :: StateTurtle Turtle
getTurtle = fst `liftM` get

putTurtle :: Turtle -> StateTurtle ()
putTurtle t = do
  (_,s) <- get
  put (t,s)
  
pop :: StateTurtle ()
pop = do  
  (x,s) <- get
  case null s of
    True  -> return ()
    False -> put (head s, tail s) 

push :: StateTurtle ()
push = do
  (x,s) <- get
  put (x,x:s)

program :: StateTurtle ()
program = lift (takeMVar mvar) >>= id >> program
    
turtleProcess = evalStateT program (turtle0, [])

send :: StateTurtle () -> IO ()
send = putMVar mvar

