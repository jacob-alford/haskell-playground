import           Data.List

type Money = Int
data CanHe = NO | YES deriving (Show,Eq)

billsMatch :: Money -> [Money] -> [Money] -> Bool
billsMatch bill reg change = billsOf change <= billsOf reg
  where billsOf = length . (filter $ (==) bill)

hasEnoughFor :: [Money] -> [Money] -> Bool
hasEnoughFor r c =
  (billsMatch 25 r c) && (billsMatch 50 r c) && (billsMatch 100 r c)

getChange :: [Money] -> Money -> [Money]
getChange _ 25 = []
getChange _ 50 = [25]
getChange register 100 | register `hasEnoughFor` [25, 50] = [25, 50]
                       | otherwise                        = [25, 25, 25]
getChange _ _ = error "error! counterfeit currency detected!"

giveChange :: [Money] -> Money -> [Money]
giveChange register customer = (customer : register) \\ change
  where change = getChange register customer

nightShift :: [Money] -> [Money] -> Int -> CanHe
nightShift customers register served
  | register `doesNotHave` change = NO
  | served + 1 == length customers = YES
  | otherwise = continueNight (giveChange register customer) (served + 1)
 where
  customer      = customers !! served
  continueNight = nightShift customers
  change        = getChange register customer
  doesNotHave reg chng = not $ reg `hasEnoughFor` chng

tickets :: [Money] -> CanHe
tickets []        = YES
tickets customers = nightShift customers [] 0
