module HW5 where

data Tree a = Tip | Bin (Tree a) a (Tree a) deriving (Show, Eq)
data Rose a = Node a [Rose a] deriving (Show, Eq)

fromTree :: Tree a -> [a]
fromTree Tip = []
fromTree (Bin left x right) = fromTree left ++ [x] ++ fromTree right

trunc :: Int -> Tree a -> Tree a
trunc i t
Â Â Â Â | i == 0 = Tip
Â Â Â Â | otherwise =
Â Â Â Â Â Â Â Â case t of
Â Â Â Â Â Â Â Â Â Â Â Â Tip -> Tip
Â Â Â Â Â Â Â Â Â Â Â Â Bin left x right -> Bin (trunc (i - 1) left) x (trunc (i - 1) right)

mirrorImage Tip Tip = True
mirrorImage (Bin l x r) (Bin ll xx rr) = if x==xx then mirrorImage l rr && mirrorImage r ll else False
mirrorImage _ _ = False

symmetric :: (Eq a) => Tree a -> Bool
symmetric Tip = True
symmetric (Bin l x r) = mirrorImage l r

sumRose :: (Num a) => Rose a -> a
sumRose (Node x xs) = x + sum (map sumRose xs)

maximumRose :: (Ord a) => Rose a -> a
maximumRose (Node x xs) = maximum (x : map maximumRose xs)

sizeRose :: Rose a -> Int
sizeRose (Node _ xs) = 1 + sum (map sizeRose xs) 

fanout :: Rose a -> Int
fanout (Node _ []) = 0
fanout (Node _ xs) = max (length xs) $ maximum $ map fanout xs

toRoses :: Tree a -> [Rose a]
toRoses Tip = []
toRoses (Bin left x right) = [Node x (toRoses left ++ toRoses right)]

fromRoses :: [Rose a] -> Tree a
fromRoses [] = Tip
fromRoses [Node a []] = Bin Tip a Tip
fromRoses [Node a [b]] = Bin (fromRoses [b]) a Tip
fromRoses [Node a [l, r]] = Bin (fromRoses [l]) a (fromRoses [r])

