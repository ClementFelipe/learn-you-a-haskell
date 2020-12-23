main :: IO ()
main = do
  print $ factorial 15
  print $ addVectors (1, 2) (3, 4)
  print $ [h | h : _ <- [[0, 0], [1, 0], [2, 0], []]] -- failed patterns in list comprehension ignored
  print $ bmiMessage 75 1.9

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (a, b) (c, d) = (a + c, b + d)

bmiMessage :: (RealFloat a) => a -> a -> String
bmiMessage w h
  | bmi <= 18.5 = "Underweight"
  | bmi <= 25.0 = "Normal"
  | bmi <= 30.0 = "Fat"
  | otherwise = "Whale"
  where
    bmi = w / h ^ 2

describeList :: [a] -> String
describeList xs =
  "The list is " ++ case xs of
    [] -> "empty."
    [_] -> "a singleton list."
    _ -> "a longer list."
