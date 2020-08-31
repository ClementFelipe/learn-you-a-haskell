main :: IO ()
main = do
  print $ doubleMe 5
  print $ doubleUs 2 5
  print $ doubleSmallNumber 100
  print $ doubleSmallNumber 101
  print $ 1 : [2, 3, 4] -- cons (:) function is O(1)
  print $ [1, 2] ++ [2, 3] -- ++ for lists of M, N sizes is O(M)
  print ("Steve Buscemi" !! 6)
  print [[1, 2, 3], [4, 5, 6, 7]] -- can have lists of lists, but must be same types
  print $ head "Pizza"
  print $ tail "Pizza"
  print $ last "Pizza"
  print $ init "Pizza"
  print $ length [1, 2, 3]
  print $ null [1, 2, 3] -- check if empty
  print $ reverse [1, 2, 3]
  print $ take 5 "Taken this is"
  print $ drop 5 "This is dropped"
  print $ sum [1, 2, 3]
  print $ product [1, 2, 3]
  print $ elem 2 [1, 2, 3] -- element in list
  print ['A' .. 'z'] -- range can work on any enumerable type
  print [1, 5 .. 20] -- [x, y..z] step is y - x, min is 1 and max is 20
  print [10, 9 .. (-10)] -- descending ranges must have step
  print $ take 11 $ cycle "LOL "
  print $ take 5 (repeat 'X') == replicate 5 'X'
  print $ [x * 2 | x <- [1 .. 10], x > 5]
  print [x | x <- [10 .. 20], x `notElem` [13, 15, 19]]
  print $ zip [1, 2] "aaaabbbb" -- truncates longer list
  print $ rightTriangles 10 24

doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100 then x else doubleMe x

rightTriangles :: (Integral a) => a -> a -> [(a, a, a)]
rightTriangles maxSide maxPerimeter = [ (a, b, c) |
  a <- [1 .. maxSide],
  b <- [1 .. maxSide],
  c <- [1 .. maxSide],
  a ^ 2 + b ^ 2 == c ^ 2,
  a + b + c == maxPerimeter ]
