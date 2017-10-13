import Data.Char

subst :: String -> String -> String -> String
subst a b c = a


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



justify :: String -> Int -> String 
justify [] n = []
justify s n
  | length(getWord s) > n = error "Word is too long!"
  | length(getWord s) <= n = getWords s n ++ "\n" ++ justify (getRestStr s (length(getWords s n))) n
    where 
      getWords :: String -> Int -> String
      getWords [] n = []
      getWords s n 
        | length(getWord s) > n = []
        | length(getWord s) == n = getWord s
        | otherwise = getWord s ++ getWords (getRestStr s (length(getWord s))) (n-length(getWord s))  

      getWord :: String -> String
      getWord [] = []
      getWord (x:[]) = [x]
      getWord (x:xs)
        | (x == ' ' || x == '\n') && not(head xs == ' ' || head xs == '\n') = []
        | otherwise = x : getWord xs

      getRestStr :: String -> Int -> String
      getRestStr [] n = []
      getRestStr (x:xs) n
        | n == 0 = xs
        | otherwise = getRestStr xs (n-1)


--overlaps :: Shape -> Shape -> Bool
--overlaps 

--loan :: Person -> Book -> ([??],[???]) -> ([??],[???])