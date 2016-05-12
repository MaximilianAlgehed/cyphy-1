{-
http://www.kestreltechnology.com/downloads/sttt-tacas02-7.pdf
-}

{-# LANGUAGE ImplicitParams #-}

module PtLTL where

import Zelus
import VBool

data F

  -- Propositional operators
  = Atomic (S VBool)  -- Atomic propositions. For example x > y.
  | Not F
  | And F F
  | Or F F
  | Imp F F

  -- Temporal operators
  | Prev F            -- In the previous step F was true.
  | Once F            -- F was true at some previous point in time.
  | Always F          -- F has always been true.
  | SinceS F F        -- F1 held at some point and since then F2 has been true.
  | SinceW F F        -- F1 has always been true or SinceS F1 F2.
  | TImp F Double

  -- Monitoring operators
  | Start F           -- F was false in the previous step but true now.
  | End F             -- F was true in the previous step but false now.
  | IntervalS F F     -- F1 was true at some point in the past and F2 not since.
  | IntervalW F F     -- If F1 was true at some point then F2 not since then.

true :: F
true = Atomic (repeat (VBool 1))

false :: F
false = Atomic (repeat (VBool (-1)))

atom :: S VBool -> F
atom = Atomic

start :: F -> F
start = Start

end :: F -> F
end = End

eval :: F -> S VBool
eval = undefined
