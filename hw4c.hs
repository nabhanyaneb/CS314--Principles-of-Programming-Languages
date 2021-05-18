import Data.List

data Poly a = Poly [a] deriving (Show, Eq)

coeffs :: (Eq a, Num a) => [a] -> Poly a
helperCoeffs [] = []
helperCoeffs s =Â Â if (last s == 0) then helperCoeffs(take (length(s)-1) s) else (s)
coeffs [] = Poly([])
coeffs s = Poly (helperCoeffs s)
--The helper function takes in the polynomial and checks if the last value is 0. If it is, then it recursively calls itself with all values except the last one until it is not. Whenever the last value is not 0, it returns the statement.--

add :: (Eq a, Num a ) => Poly a -> Poly a -> Poly a
findLength s l = if (length s >= l) then s else findLength (0:s) l
helperAdd (Poly s1) (Poly s2) = reverse(zipWith (+) (findLength (reverse s1) biggestExp) (findLength (reverse s2) biggestExp)) where biggestExp = max (length s1) (length s2)
add (Poly s1) (Poly s2) = if length (helperAdd (Poly s1) (Poly s2)) == 0 then Poly[] else coeffs (helperAdd (Poly s1) (Poly s2))
-- Once the statements are the same length we must use zipWidth (+) to add them together. Reverse is used a few times to account for left-to-right addition and the answer being in the correct format. findLength is used to add 0 to the front of the list until necessary. biggestExp is used to find the legnth of the longer statement, which is what findLength uses to match the lengths-- 

at :: (Eq a, Num a ) => Poly a -> a -> a
convert (Poly s) = s
evaluate n = foldr (\u v -> u +v*n) 0
at (Poly s) n = evaluate n (convert (Poly s))
--The statement is evaluated using the basic defintion of a polynomial:  a + b*x + c*x^2 + d*x^3 + ... = a + x*(b + x*(c + x*(d + x*(â€¦)))). This is clearly in a format for foldr (as referenced from our notes) so foldr is used on the function (where z is 0).-- 

neg :: (Eq a, Num a) => Poly a -> Poly a
f x = (-1) * x
convert2 (Poly p) = p
neg (Poly s) = coeffs(map f (convert2 (Poly s)))

mult :: (Eq a, Num a) => Poly a -> Poly a -> Poly a
mul [] _ = [0]
mul _ [] = [0]
mul xs ys = convert2 (add (Poly(map (*(head xs)) ys)) (Poly((0 : (mul (tail xs) ys)))))
mult (Poly p1) (Poly p2) = coeffs (mul (p1) (p2))

instance (Num a, Eq a) => Num (Poly a) where    
    (+) = add
    negate = neg
    (*) = mult
    fromInteger 0 = Poly []
    fromInteger n = Poly [fromInteger n]
    abs = error "No abs for Poly"
    signum = error "No signum for Poly"

x :: (Num a, Eq a) => Poly a
x = Poly[0,1]

y :: (Num a, Eq a) => Poly (Poly a)
y = Poly [x]

-- Yes, I get a sensible answer. I set x = Poly[0,1] to test it out -- 