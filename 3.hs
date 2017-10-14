import Data.Char

-- Exercise 1
subst :: String -> String -> String -> String
subst a b c = a

-- Exercise 2
isPalin :: String -> Bool
isPalin [] = True
isPalin str
  | length(strIgn) `mod` 2 == 1 = comp (take (length(strIgn) `div` 2) strIgn) (drop ((length(strIgn) `div` 2)+1) strIgn)
  | length(strIgn) `mod` 2 == 0 = comp (take (length(strIgn) `div` 2) strIgn) (drop (length(strIgn) `div` 2) strIgn)
  where 
    strIgn = capitalsIgnored(str)


comp :: String -> String -> Bool
comp str1 str2 
  | str1 == reverse(str2) = True
  | otherwise = False

capitalsIgnored :: String -> String
capitalsIgnored str = [toLower x | x <- punctuationNotIncluded str]

punctuationNotIncluded:: String -> String
punctuationNotIncluded (x:xs)
  | xs == [] && elem x punctuation = []
  | xs == [] && not(elem x punctuation) = [x]
  | elem x punctuation = punctuationNotIncluded xs
  | otherwise = x : punctuationNotIncluded xs
  where 
      punctuation = ['.',',',';','-',':'] ++ [' ','\n','\t']

-- Exercise 3
count :: String -> (Int, Int, Int)
count [] = (0,0,0)
count s = (c, w, l)
  where 
    c = length s
    w = noOfWords (dropFrontSpaces(dropEndSpaces s))
    l = noOfLines s

dropFrontSpaces :: String -> String
dropFrontSpaces [] = []
dropFrontSpaces (x:xs)
  | x == ' ' || x == '\n' = dropFrontSpaces xs
  | otherwise = x:xs

dropEndSpaces :: String -> String
dropEndSpaces [] = []
dropEndSpaces (x:xs)
  | xs == [] && (x == ' ' || x == '\n') = []
  | xs == [] && x /= ' ' && x /= '\n' = [x]
  | (x == ' ' || x == '\n') && (head xs == ' ' || head xs == '\n') = dropEndSpaces xs
  | otherwise = x : dropEndSpaces xs

noOfWords :: String -> Int
noOfWords str = length words + 1
  where words = [xx | xx <- str, xx == ' ' || xx == '\n']

noOfLines :: String -> Int
noOfLines str = (length newLines) + 1
  where newLines = [xx | xx <- str, xx == '\n']


-- Exercise 4
justify :: String -> Int -> String
justify s n = init(justify' s n) 

-- 't'and 'f' solves problem then spaces are before first word
-- 'f' means that there was only spaces before, 't' - that char was found
justify' :: String -> Int -> String 
justify' [] _ = []
justify' s n
  | n < 1 = "Invalid number"
  | length(getWord s 'f') > n = error "Word is too long!"
  | length(getWord s 'f') <= n = getWords s n ++ "\n" ++ justify' (getRestStr s (length(getWords s n))) n
    where 
      getWords :: String -> Int -> String
      getWords [] n = []
      getWords _ 0 = []
      getWords s n 
        | length(getWord s 'f') > n = []
        | length(getWord s 'f') == n = getWord s 'f'
        | otherwise = getWord s 'f' ++ getWords (getRestStr s (length(getWord s 'f'))) (n-length(getWord s 'f'))  

      getWord :: String -> Char -> String
      getWord [] _ = []
      getWord (x:xs) c
        | (c == 'f') && (x == ' ' || x == '\n') && not(head xs == ' ' || head xs == '\n') = x : getWord xs 'f'
        | (x == ' ' || x == '\n') && not(head xs == ' ' || head xs == '\n') = [x]
        | otherwise = x : getWord xs 't'

      getRestStr :: String -> Int -> String
      getRestStr [] n = []
      getRestStr (x:xs) n
        | n == 0 = x:xs
        | otherwise = getRestStr xs (n-1)

-- 5 Exercise
-- overlaps (Circle 2 (2,2)) (Circle 2 (1,2))
-- overlaps (Rectangle 2 2 (1,1)) (Rectangle 2 2 (1,1))
-- overlaps (Circle 2 (2,2)) (Rectangle 2 2 (1,1))
-- overlaps (Rectangle 2 2 (2,2)) (Circle 2 (2,2))
data Shape = Circle Float (Int,Int) | Rectangle Float Float (Int,Int) 
  deriving (Show, Ord, Eq)

overlaps :: Shape -> Shape -> Bool
overlaps (Circle c1 (x1,y1)) (Circle c2 (x2,y2)) = compares (coord(Circle c1 (x1,y1))) (coord(Circle c2 (x2,y2)))
overlaps (Rectangle w1 h1 (x1,y1)) (Rectangle w2 h2 (x2,y2)) =  compares (coord(Rectangle w1 h1 (x1,y1)))  (coord(Rectangle w2 h2 (x2,y2)))
overlaps (Circle c (x1,y1)) (Rectangle w h (x2,y2)) = compares (coord(Circle c (x1,y1))) (coord(Rectangle w h (x2,y2)))
overlaps (Rectangle w h (x1,y1)) (Circle c (x2,y2)) = compares (coord(Rectangle w h (x1,y1))) (coord(Circle c (x2,y2)))
 
compares :: (Float,Float,Float,Float) -> (Float,Float,Float,Float) -> Bool
compares (x1,x2,y1,y2) (xx1,xx2,yy1,yy2) 
  | (x1 > xx1 && x1 < xx2 && y1 > yy1 && y1 < yy2) || (x2 > xx1 && x2 < xx2 && y2 > yy1 && y2 < yy2) = True
  | otherwise = False

coord :: Shape -> (Float,Float,Float,Float)
coord (Rectangle w h (x,y)) = ((fromIntegral x), (fromIntegral x)+w, (fromIntegral y), (fromIntegral y)+h)
coord (Circle r (x,y)) = ((fromIntegral x)-r, (fromIntegral x)+r, (fromIntegral y)-r, (fromIntegral y)+r)


-- Exercise 6
--loan :: Person -> Book -> ([??],[???]) -> ([??],[???])