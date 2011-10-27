{-# LANGUAGE FlexibleInstances #-}

module Main where

import SimpleDrawing

main = run $ draw $ scale xs 0.18 where
  xs :: [[Point]]
  xs = [take 1000 (orb g (x,0)) | x <- [-3,-2.95..3]]

g (x,y) = let x1 = x + y1
              y1 = y + k * sin x
              k  = sqrt 2
          in (x1,y1)
             
orb :: (a -> a) -> a -> [a]
orb g p = scanl (\x f -> f x) p (repeat g)
