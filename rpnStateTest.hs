type TapeItem = (String, String)

data RPNState = RPNState { stack :: [Double]
                         , tape :: [TapeItem]
                         } deriving (Show, Eq)

data RPNHist = RPNHist { notStashed :: [RPNState]
                       , stashed :: [RPNState]
                       } deriving (Show, Eq)

data Op = Add | Sub | Mul | Div | Pow

mkOp :: (a -> b -> c) -> (a -> b -> d) -> a -> b -> (c, d)
mkOp f g a b = (f a b, g a b)

ops :: Op -> Double -> Double -> (Double, String)
ops op = case op of
  Add -> mkOp (+) (\a b -> show a ++ "+" ++ show b)
  Sub -> mkOp (-) (\a b -> show a ++ "-" ++ show b)
  Mul -> mkOp (*) (\a b -> show a ++ "*" ++ show b)
  Div -> mkOp (/) (\a b -> show a ++ "/" ++ show b)
  Pow -> mkOp (**) (\a b -> show a ++ "^" ++ show b)

data Sop = Exp | Sin | Cos | Tan | Asin | Acos | Atan

mkSop :: (a -> b) -> (a -> c) -> a -> (b, c)
mkSop f g a = (f a, g a)

sop :: Sop -> Double -> (Double, String)
sop op = case op of
  Exp  -> mkSop exp (\a -> "e^" ++ show a)
  Sin  -> mkSop sin (\a -> "sin(" ++ show a ++ ")")
  Cos  -> mkSop cos (\a -> "cos(" ++ show a ++ ")")
  Tan  -> mkSop tan (\a -> "tan(" ++ show a ++ ")")
  Asin -> mkSop asin (\a -> "asin(" ++ show a ++ ")")
  Acos -> mkSop acos (\a -> "acos(" ++ show a ++ ")")
  Atan -> mkSop atan (\a -> "atan(" ++ show a ++ ")")

data RPNOp = Op | Sop

sOp :: RPNState -> Sop -> RPNState
sOp RPNState { stack = st, tape = tp } op = newState
 where
  result   = sop op (st !! 0)
  newStack = fst result : drop 1 st
  newTape  = (snd result, (show . fst) result) : tp
  newState = RPNState { stack = newStack, tape = newTape }

dOp :: RPNState -> Op -> RPNState
dOp RPNState { stack = st, tape = tp } op = newState
 where
  result   = ops op (st !! 0) (st !! 1)
  newStack = fst result : drop 1 st
  newTape  = (snd result, (show . fst) result) : tp
  newState = RPNState { stack = newStack, tape = newTape }

operate :: RPNState -> RPNOp -> RPNState
operate RPNState { stack = [], tape = _ } _ =
  error "Unable to reduce empty stack!"
operate state op = case op of
  Op  -> dOp state op
  Sop -> sOp state op

