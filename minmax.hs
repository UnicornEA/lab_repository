mymax (x:xs) = foldr max x xs
  where max a b | a > b = a
                | otherwise = b

mymin (x:xs) = foldr min x xs
  where min a b | a < b = a
                | otherwise = b

getMinAndMax (x:xs) = [mymax(x:xs), mymin(x:xs)]