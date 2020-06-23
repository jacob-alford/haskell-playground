shape :: Int -> [a] -> [[a]]
shape _ [] = []
shape n arr | length arr < n = error "Provided shape does not divide array!"
            | otherwise      = [take n arr] ++ shape n (drop n arr)

offDiag :: Int -> [[a]] -> a
offDiag index mat = mat !! index !! offIndex
  where offIndex = (length mat - 1) - index

diag :: Int -> [[a]] -> a
diag rowCol mat = mat !! rowCol !! rowCol

getDiags :: [[a]] -> [a]
getDiags matrix = [ ith `diag` matrix | (_, ith) <- matrix `zip` [0 ..] ]

getOffDiags :: [[a]] -> [a]
getOffDiags matrix = [ ith `offDiag` matrix | (_, ith) <- matrix `zip` [0 ..] ]

diagDiff :: [[Int]] -> Int
diagDiff matrix = abs $ (sum $ getDiags matrix) - (sum $ getOffDiags matrix)

