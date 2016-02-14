module Plot where

import Data.List( intersperse )
import System.Process( system )

import Zelus

--------------------------------------------------------------------------------
-- plotting streams

plot :: FilePath -> Int -> [(String,S Double)] -> IO ()
plot file n ys =
  do sequence_
       [ writeFile (file ++ "_" ++ name ++ "_.xy") $ unlines $
           [ show x ++ " " ++ show v
           | (x,v) <- [0..] `zip` take n y
           ]
       | (name,y) <- ys
       ]
     writeFile (file ++ "_gnuplot_.in") $ unlines $
       [ "set terminal png enhanced font 'Times-Roman,18'"
       , "set grid"
       , "set output '" ++ file ++ "_plot_.png'"
       , "plot " ++
           concat (intersperse ", "
           [ "'" ++ file ++ "_" ++ name ++ "_.xy' with lines title '" ++ name ++ "'"
           | (name,_) <- ys
           ])
       ]
     system ("gnuplot < '" ++ file ++ "_gnuplot_.in'")
     return ()
     
--------------------------------------------------------------------------------

