--Exercise 1.
average :: [Float] -> Float
average list = sum list / fromIntegral(length list)


--Exercise 2.
dividesC :: Integer -> [Integer]
dividesC number = [n | n <- [1..number], mod number n == 0]


divides :: Integer -> [Integer]
divides number  
  | number > 0 =  [number] ++ divides(number - 1)
  | number == 0 = []
--  | otherwise 
 
isprime :: Integer -> Bool
isprime number = False


--Exercise 3.
prefix :: String -> String -> Bool
prefix str1 str2 
  | head str1 == head str2 = True
  | otherwise = False

substring :: String -> String -> Bool
substring str1 str2 = False
-- | otherwise = False


--Exercise 4.
permut :: [Integer] -> [Integer] -> Bool
permut list1 list2
  | elem (head list1) list2 = True
  | otherwise = False


--Exercise 5.
capitalise :: String -> String
capitalise str  = [x | x <- str, chars <- (['a'..'z'] ++ ['A'..'Z']), x==chars]


--Exercise 6.
itemtotal :: [(String, Float)] -> [(String, Float)]
itemtotal [(s,f)] = [x | x <- [(s,f)]]


itemDiscount :: String -> Integer -> [(String, Float)] -> [(String, Float)]
itemDiscount name number [(name1, number1)] = [(name1, number1)]
