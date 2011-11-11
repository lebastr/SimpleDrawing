{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, DeriveFunctor, IncoherentInstances #-}

module SimpleDrawing ( draw
                     , run
                     , GLfloat 
                     , Point
                     , Drawable 
                     , Polygon(..)
                     , Line(..)
                     , Circle(..)
                     , Triangle(..)
                     , fromPolar2D 
                     , Rotate 
                     , rotate
                     , Scale
                     , scale 
                     , flush 
                     , run1 
                     , run2
                     , mainLoop 
                     , GLGraphics 
                     , clear 
                     , currentColor
                     , ($=)
                     , Color4 (..)
                     , Radius 
                     , Angle ) where

import Graphics.UI.GLUT hiding (Point, Polygon, Line, Triangle, rotate, scale, clear)
import qualified Graphics.UI.GLUT as GL
import Control.Concurrent

clear = do
  GL.clear [GL.ColorBuffer]
  flush

{- 1. Посмотреть про полигоны и для чего нужен PolygonMode

-}

type Point = (GLdouble, GLdouble)
type GLGraphics a = IO a

data Polygon p = Polygon [p] deriving (Show, Functor)

data Line p = Line p p deriving (Show, Functor)

data Circle p = Circle p Radius deriving (Show, Functor)

data Triangle p = Tr p p p deriving (Show, Functor)

type Polar2D = (Radius, Angle)

fromPolar2D :: Polar2D -> Point
fromPolar2D (r,a) = (r*cos a, r*sin a)

class Drawable a where
  draw :: a -> GLGraphics ()

instance Drawable a => Drawable [a] where
  draw = mapM_ draw
  
instance Drawable Point where
  draw (x,y) = renderPrimitive Points $ vertex $ Vertex2 x y
  
instance Drawable (Line Point) where
  draw (Line (x0,y0) (x1,y1)) = renderPrimitive Lines $ do
    vertex $ Vertex2 x0 y0
    vertex $ Vertex2 x1 y1

instance Drawable (Polygon Point) where
  draw (Polygon ps) = renderPrimitive LineLoop $ mapM_ (vertex . uncurry Vertex2) ps

instance Drawable (Circle Point) where
  draw (Circle (a,b) r) = draw $ Polygon $ map (\(x,y) -> (x+a, y+b)) [fromPolar2D (1,t) | t <- [0,pi/24..2*pi]]

instance Drawable (Triangle Point) where
  draw (Tr p0 p1 p2) = draw $ Polygon [p0,p1,p2]

class Rotate a where
  rotate :: a -> Angle -> a

-- Почему происходит overlapping??? 

instance Rotate Point where
  rotate (x,y) a = let x1 = x*cos a - y*sin a
                       y1 = x*sin a + y*cos a
                   in (x1,y1)

instance (Rotate p, Functor f) => Rotate (f p) where
  rotate p a = fmap (\x -> rotate x a) p 

class Scale a where
  scale :: a -> GLdouble -> a
  
instance Scale Point where
  scale (x,y) m = (m*x, m*y)
  
instance (Functor f, Scale p) => Scale (f p) where
  scale p m = fmap (\x -> scale x m) p

run :: IO () -> IO ()
run io = do
  getArgsAndInitialize
  createWindow "App"
  displayCallback $= do
    GL.clear [ColorBuffer]
    currentColor $= Color4 0 0.3 1 1
    io
    flush
  mainLoop

run1 :: IO () -> IO ()
run1 io = do
  getArgsAndInitialize
  createWindow "App"
  displayCallback $= do
    GL.clear [ColorBuffer]
    currentColor $= Color4 0 0.3 1 1
    io
    flush

run2 :: MVar (IO ()) -> IO ()
run2 mvar = do
  getArgsAndInitialize
  createWindow "App"
  -- GL.clear [ColorBuffer]
  -- currentColor $= Color4 0 0.3 1 1
  -- flush
  idleCallback $= Just m
  displayCallback $= do
    GL.clear [ColorBuffer]
    currentColor $= Color4 0 0.3 1 1
    flush
    -- print "DisplayCallback"
--  mainLoop
--  keyboardMouseCallback $= Just (\_ _ _ _ -> print "Click")
  where
    m1 = takeMVar mvar >>= id
    m = do
      v <- tryTakeMVar mvar
      case v of
        Just io -> io
        Nothing -> return ()