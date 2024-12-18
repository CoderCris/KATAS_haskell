module SquareDigit where
import Data.Char

squareDigit :: Int -> Int
squareDigit value =  
  let digits = map digitToInt (show value)
      squares = map (^2) digits
      concat = concatMap show squares
  in read concat :: Int
