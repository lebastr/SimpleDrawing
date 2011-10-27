import SimpleDrawing
import Control.Concurrent
import System.IO.Unsafe (unsafePerformIO)
-- import IO

mvar :: MVar Char
mvar = unsafePerformIO newEmptyMVar

step = 0.03

process = run1 $ prog (0,0,0) where
  prog (x,y,a) = do
    draw ((x,y) :: Point)
    flush
    k <- takeMVar mvar
    case k of 
      'h' -> prog (x-step, y, a) 
      'l' -> prog (x+step, y, a) 
      'k' -> prog (x, y+step, a) 
      'j' -> prog (x, y-step, a) 
      _   -> prog (x,y,a)
  --  let (x,y) = fromPolar2D (1-1/t,t)
--    draw (x,y)
    
    

main = do
  process
  forkIO mainLoop
  
drive :: [Char] -> IO ()
drive = mapM_ (putMVar mvar) 

send :: IO [Char]
send = do
  k <- getChar
  case k of
    '\n' -> return []
    _    -> do
      drive [k]
      ks <- send
      return (k:ks)
