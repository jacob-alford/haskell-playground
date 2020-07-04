module CreatePhoneNumber where

toString :: [Int] -> String
toString = (filter (`notElem` "[,]")) . show

areaCode :: [Int] -> String
areaCode = toString . (take 3)

centralOffice :: [Int] -> String
centralOffice = toString . (take 3) . (drop 3)

subNum :: [Int] -> String
subNum = toString . (take 4) . (drop 6)

createPhoneNumber :: [Int] -> String
createPhoneNumber p =
  '(' : areaCode p ++ ')' : ' ' : centralOffice p ++ '-' : subNum p
