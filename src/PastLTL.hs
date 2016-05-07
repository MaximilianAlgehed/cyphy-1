{-# LANGUAGE ImplicitParams #-}

module PastLTL where

import Zelus
import CyphyUtils

since :: S Bool -> S Bool -> S Bool
this `since` that = always ((nt (once that)) ||? this)

once :: S Bool -> S Bool
once (False:this) = False:once this
once (True:this) = repeat True

always :: S Bool -> S Bool
always (True:this) = True:always this
always (False:this) = repeat False

timer :: (?h :: Double) => Double -> S Bool -> S Bool
timer delay resets = tim 0 resets
  where
    tim del (True:res) = True:tim (delay - ?h) res
    tim del (False:res) =
      if del > 0 then True:tim (del - ?h) res else False:tim del res

ex1 = every 5
ex2 = replicate 10 True ++ ex1
ex3 = replicate 7 False ++ [True] ++ repeat False
ex4 = take 7 ex1 ++ repeat True
ex5 = nt ex3
