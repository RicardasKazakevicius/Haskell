import Test.QuickCheck

-- Exercise 1:
nAnd :: Bool -> Bool -> Bool
nAnd x y = not(x && y)

nAnd1 :: Bool -> Bool -> Bool
nAnd1 True x = not(x)
nAnd1 False x = x || not(x)

nAnd2 :: Bool -> Bool -> Bool
nAnd2 True True = False
nAnd2 True False = True
nAnd2 False True = True
nAnd2 False False = True


--Exercise 2:
--Check!
prop_nAnd :: Bool -> Bool -> Bool
prop_nAnd x y = (nAnd x y == nAnd2 x y) || (nAnd2 x y)


--Exercise 3:
-- ? guard if f definition
nDigits :: Integer -> Int
nDigits x = length(show x)


--Exercise 4:
nRoots :: Float -> Float -> Float -> Int
nRoots a b c
  | a == 0.0 = error "the first argument should be non-zero!"
  | b*b > 4.0*a*c = 2
  | b*b == 4.0*a*c = 1
  | b*b < 4.0*a*c = 0


--Exercise 5:
smallerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c 
  | nRoots a b c == 1 = ( (-b) - sqrt( (b*b -4*a*c)) ) / (2*a) 
  | nRoots a b c == 2 = ( (-b) - sqrt( (b*b -4*a*c)) ) / (2*a)
  | nRoots a b c == 0 = error "No root!"

largerRoot :: Float -> Float -> Float -> Float
largerRoot a b c
  | nRoots a b c == 1 = ( (-b) + sqrt( (b*b -4*a*c)) ) / (2*a) 
  | nRoots a b c == 2 = ( (-b) + sqrt( (b*b -4*a*c)) ) / (2*a)
  | nRoots a b c == 0 = error "No root!"


--Exercise 6:
power2 :: Integer -> Integer
power2 n 
  | n < 0 = 0
  | n == 0 = 1
  | n > 0 = 2*power2(n-1)
 

--Exercise 7:
mult :: Integer -> Integer -> Integer
mult m n 
  | n < 0 = -m + mult m (n+1)
  | n > 0 = m + mult m (n-1)
  | n == 0 = 0


--Exercise 8:
prod :: Integer -> Integer -> Integer 	
prod m n
  | m > n =  error "Invalid numbers!"
  | m < n = m * prod (m+1) n
  | m == n = n