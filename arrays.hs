getLargerNumbers :: Ord a => [a] -> [a] -> [a]
getLargerNumbers arr1 arr2 = map (\(x, y) -> max x y) (zip arr1 arr2)

powersOfTwo :: Int -> [Int]
powersOfTwo n = map (\x -> 2^x) [0..n]

distancesFromAverage :: [Double] -> [Double]
distancesFromAverage list = 
  let avg = average list 
  in map (\x ->  (roundTo2 (x - avg)) * (-1) ) list 

average :: [Double] -> Double
average list = sum list / fromIntegral (length list)

roundTo2 :: Double -> Double
roundTo2 x = fromIntegral (round (x * 100)) / 100 
