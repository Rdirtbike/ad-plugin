{-# OPTIONS_GHC -fplugin=MyLib #-}

module Test (f) where

import MyLib

{-# ANN f (Diff "f'") #-}
f :: Double -> Double
f _ = 1
