module New where

import Numeric.GSL.Minimization

hej xs = sum (map sq (zipWith (+) xs [1.0,2.0..])) where sq x = x*x

opt = minimize NMSimplex 0.01 10000 (replicate n 1.0) hej (replicate n 0.0)

n = 2

