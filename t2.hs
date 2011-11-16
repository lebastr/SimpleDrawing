module Main where

import SimpleDrawing
import Control.Concurrent
import qualified Turtle as T
import Control.Monad
import Control.Monad.State.Strict
    
tree1 n = T.send $ tree (30,0.8) (20,0.85) n

tree a b n = evalStateT (treeCS a b n) colors

monoColorTree n = (T.color `liftM` T.getTurtle) 
                  >>= evalStateT (treeCS (30, 0.8) (20, 0.85) n) . repeat

tree2 = T.send . monoColorTree

example1 = replicateM_ 12 (p >> turn 30) where
  p = replicateM_ 12 (forward >> turn 30)
  
example2 = cstep 2 >> tree1 12

type ST = StateT [T.MyColor] T.StateTurtle

treeCS :: (T.Angle,T.Distance) -> (T.Angle,T.Distance) -> Int -> ST ()
treeCS (a0,s0) (a1,s1) n | n == 0 = return ()
                         | otherwise = do
  (c:cs) <- get
  put cs
  lift $ do 
    T.dippen c
    T.forward
    T.push
    T.turn (-a0)
    T.cstep s0
  treeCS (a0,s0) (a1,s1) (n-1)
  lift $ do
    T.pop 
    T.push
    T.turn a1
    T.cstep s1
  treeCS (a0,s0) (a1,s1) (n-1)
  lift T.pop

main = do
  run2 T.mvarIO
  forkIO T.turtleProcess
  forkIO mainLoop
  
cstep   = T.send . T.cstep
forward = T.send T.forward
turn    = T.send . T.turn
pop     = T.send T.pop
push    = T.send T.push
reset   = T.send T.reset
clean   = T.send T.clean
repl n  = replicateM_ n
penup   = T.send T.penup
pendown = T.send T.pendown
dippen  = T.send . T.dippen

colors :: [T.MyColor]
colors = concat $ repeat cs
  where
    cs  = zipWith (\a b -> (a,b,0)) xs' ys'
    xs' = xs ++ replicate 10 1.0
    ys' = replicate 10 1.0 ++ reverse xs
    xs  = [0,0.1..1.0]

example3 = T.send (T.setpos (-0.2,-0.9)) 
           >> cstep 2.5 
           >> mapM_ (\c -> h c fig) colors
  where
    fig = cstep 0.2 >> replicateM_ 5 (replicateM_ 6 (forward >> turn 60) >> turn 72)
    h c fig = penup >> dippen c >> push >> pendown >> fig >> pop >> forward >> turn 16 >> cstep 0.99

--fun :: Double -> Double
fun s | s >= 0 && s < 1/6 = 1.0
      | s >= 1/6 && s < 1/3 = 2 - 6*s
      | s >= 1/3 && s < 2/3 = 0.0
      | s >= 2/3 && s < 5/6 = 6*s - 4
      | s >= 5/6 && s < 1 = 1.0
      | otherwise = fun s' where s' = s - (fromIntegral $ floor s)

color :: GLfloat -> T.MyColor
color angle = let s = angle / 360
                  r = fun s
                  g = fun (s - 1/3)
                  b = fun (s - 2/3)
              in (r,g,b)

example4 = do
  clean >> reset >> T.send (T.setpos (-0.2,-0.8)) >> cstep 2.7
  sequence_ $ zipWith sp (cycle [cstep 0.3 >> turn (-90) >> tree2 8]) cs
  where
    sp p c = penup >> push >> pendown >> dippen c >> p >> pop >> forward >> turn 16 >> cstep 0.99
    cs = concat $ repeat $ map color (s ++ reverse s)
    s = [-180, -170..(-120)]

example5 = cstep 0.04 >> (h1.h1'.a.a.a.a) forward
  where
    h1 x = repl 12 (x >> turn 30)
    h1' x = mapM_ (\c -> dippen c >> x >> turn 30) $ 
            take 12 [color x | x <- [0, 30..360]]
    a x = x >> turn 60 >> x >> turn (-120) >> x >> turn 60 >> x
