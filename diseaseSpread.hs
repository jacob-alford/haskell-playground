module Codewars.G964.Epidemy where

type EpiSnap = (Double, Double, Double)

hd :: EpiSnap -> Double
hd (a, _, _) = a

nextSnap :: Double -> Double -> Double -> EpiSnap -> EpiSnap
nextSnap dt b a (cInf, cSus, cRec) = (nxtInf, nxtSus, nxtRec)
 where
  nxtInf = cInf + dt * (b * cSus * cInf - a * cInf)
  nxtSus = cSus - dt * b * cSus * cInf
  nxtRec = cRec + dt * cInf * a

epidemic :: Int -> Int -> Double -> Double -> Double -> Double -> Int
epidemic tm n s0 i0 b a = (ceiling . hd . maximum)
  $ scanl (\hist _ -> nextSnap dt b a hist) (i0, s0, 0) [0, dt .. days]
 where
  days = fromIntegral tm
  dt   = days / fromIntegral n
