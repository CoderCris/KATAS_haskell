getLargerNumbers :: Ord a => [a] -> [a] -> [a]
getLargerNumbers arr1 arr2 = map (\(x, y) -> max x y) (zip arr1 arr2)

powersOfTwo :: Int -> [Int]
powersOfTwo n = map (\x -> 2^x) [0..n]