import Data.Char
import Data.List

-- Global names
places  = ["","Thousand","Million","Billion","Trillion","Quadrillion","Quintillion"]
ones    = ["One","Two","Three","Four","Five","Six","Seven","Eight","Nine"]
teens   = ["Ten","Eleven","Twelve","Thirteen","Fourteen","Fifteen","Sixteen","Seventeen","Eighteen","Nineteen"]
tens    = ["A 1 made it to getTens","Hundred","Twenty","Thirty","Forty","Fifty","Sixty","Seventy","Eighty","Ninety"]

-- Word lookup functions
getPlaceName :: Int -> String
getPlaceName place = places !! place

getOnes :: Char -> String
getOnes '0' = ""
getOnes n = let index = (digitToInt n) - 1 in
    ones !! index

getTeens :: String -> String
getTeens n = let r = read n :: Int in
    head $ [x | (i,x) <- (zip [10..] teens), i == r]

getTens :: Char -> String
getTens '0' = ""
getTens n   = let index = (digitToInt n) in
    tens !! index

--Takes a string rep. of a number, reverses it, and groups it into 3-groups
-- 12,000 -> ["000", "21"]
cutIntoGroups :: String -> [String]
cutIntoGroups = reverse . cut [] . reverse
    where
        cut acc []      = acc
        cut acc rest    = let (next,rest') = splitAt 3 rest in
            cut (next : acc) rest'

-- Applies the "rules" to each sub-chunk 
applyLocalRules :: String -> String
applyLocalRules revChunk = hund ++ tensNOnes
    where
        (ones,tens,hunds) = toTriple revChunk '0'
        tensNOnes = if tens == '1' then (getTeens [tens,ones])
                        else (getTens tens) ++ (getOnes ones)
        hund = if hunds == '0' then ""
                    else (getOnes hunds) ++ "Hundred"

-- Succeedes each formatted chunk with the place name                   
applyRules :: [String] -> [String]
applyRules = reverse . map (\(i,g) -> let pName = getPlaceName i in
    applyLocalRules g ++ pName) . zip [0..]

toTriple :: [a] -> a -> (a,a,a)
toTriple [] def = (def,def,def)
toTriple xs def = case xs of
    [x,y,z] -> (x,y,z)
    [x,y]   -> (x,y,def)
    [x]     -> (x,def,def)
    _       -> error "List greater then 3"

addCommasAndConcat :: [String] -> String
addCommasAndConcat = concat . map (\chunk -> chunk ++ ",")

-- Adds spaces before words
addSpaces :: String -> String
addSpaces [] = []
addSpaces left = (h : pre) ++ " " ++ addSpaces post
    where
        (h:t)       = left
        (pre,post)  = break isUpper t

-- Loosely stolen from SO       
trim :: String -> String
trim = let t = reverse . dropWhile (not . isAlpha) in
    t . t

-- Main function    
convert :: Int -> String
convert = trim . addSpaces . addCommasAndConcat
    . applyRules . cutIntoGroups . show
	
	

-- No Limiations