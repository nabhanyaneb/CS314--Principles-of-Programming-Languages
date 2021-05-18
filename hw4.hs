import Data.List

data Poly = Poly [Double] deriving (Show, Eq)

helperCoeffs [] = []
helperCoeffs s =Â Â if (last s == 0) then helperCoeffs(take (length(s)-1) s) else (s)
coeffs [] = Poly([])
coeffs s = Poly (helperCoeffs s)

add :: Poly -> Poly -> Poly
findLength s l = if (length s >= l) then s else findLength (0:s) l
helperAdd (Poly s1) (Poly s2) = reverse(zipWith (+) (findLength (reverse s1) biggestExp) (findLength (reverse s2) biggestExp)) where biggestExp = max (length s1) (length s2)
add (Poly s1) (Poly s2) = if length (helperAdd (Poly s1) (Poly s2)) == 0 then Poly[] else coeffs (helperAdd (Poly s1) (Poly s2))

at :: Poly -> Double -> Double
convert (Poly s) = s
evaluate n = foldr (\u v -> u +v*n) 0
at (Poly s) n = evaluate n (convert (Poly s))

neg :: Poly -> Poly
f x = (-1) * x
convert2 (Poly p) = p
neg (Poly s) = coeffs(map f (convert2 (Poly s)))

mult :: Poly -> Poly -> Poly
mul [] _ = [0]
mul _ [] = [0]
mul xs ys = convert2 (add (Poly(map (*(head xs)) ys)) (Poly((0 : (mul (tail xs) ys)))))
mult (Poly p1) (Poly p2) = coeffs (mul (p1) (p2))

instance Num Poly where
    (+) = add
    negate = neg
    (*) = mult
    fromInteger 0 = Poly []
    fromInteger n = Poly [fromInteger n]
    abs = error "No abs for Poly"
    signum = error "No signum for Poly"

x = Poly[0,1]
-- Yes, I get a sensible answer. I set x = Poly[0,1] to test it out -- 