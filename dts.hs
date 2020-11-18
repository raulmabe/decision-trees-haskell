-- UTILITIES
transpose               :: [[a]] -> [[a]]
transpose []             = []
transpose ([]   : xss)   = transpose xss
transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])

removeChar :: String -> String -> String
removeChar c s = filter (not . (`elem` c)) s

allSame :: Eq a => [a] -> Bool
allSame s = all (== head s) s

removeTupleWhere :: Eq a => a -> [(a,b)] -> [(a,b)]
removeTupleWhere n ts = filter (\tuple -> fst tuple /= n) ts

nub :: Eq a => [a] -> [a]
nub (x:xs) = x : nub (filter (/= x) xs)
nub [] = []

classCounts :: Row -> [(String,Int)]
classCounts row = map (`classCounts'` row) $ nub row

classCounts' :: String -> Row -> (String,Int)
classCounts' a row = (a, length $ filter (== a) row)

gini :: Row -> Float
gini row = gini' (length row)  (classCounts row) 1 

gini' :: Int -> [(String,Int)] -> Float  -> Float
gini' length appears impurity 
  | null appears = impurity
  | otherwise = gini' length (tail appears) $ impurity - ((fromIntegral (snd (head appears)) / fromIntegral length) ** 2.0)


infoGainList :: Dataset -> [Float]
infoGainList (header, matrix) = map calcInfoGain $ tail $ zip [0..] header
  where calcInfoGain attrWithIndex = infoGain attrWithIndex matrix

infoGain :: (Int, Attribute) -> [Row] -> Float
infoGain level matrix = genericGini - avgGini level matrix
  where genericGini = gini $ head $ transpose matrix

avgGini :: (Int, Attribute) -> [Row] -> Float
avgGini (index, attr) matrix = foldl calcula 0 (map (search partition index matrix) (snd attr))
  where calcula avg (quantity, gini) = avg + ((fromIntegral quantity / fromIntegral (length $ head $ transpose matrix)) * gini)


partition :: Int -> [Row] -> Dict
partition index matrix = foldl insereix (create (0,-1)) (nub (transpose matrix !! index))
  where insereix dict key = insert dict key value
          where value =  (length (head (transpose filteredTable)), gini (head (transpose filteredTable)))
                  where filteredTable = filter (\row -> row !! index == key) matrix


filteredRows :: Int -> String -> [Row] -> [Row]
filteredRows index key  = filter (\row -> row !! index == key) 

getMaxWithIndex :: [Float] -> (Float, Int)
getMaxWithIndex xs = getMaxWithIndex' xs 0 0 0

getMaxWithIndex' :: [Float] -> Int -> Float -> Int -> (Float,Int)
getMaxWithIndex' llista currentIndex max maxIndex
  | length llista == currentIndex = (max,maxIndex)
  | (llista !! currentIndex) > max = getMaxWithIndex' llista (currentIndex+1) (llista !! currentIndex) currentIndex
  | otherwise = getMaxWithIndex' llista (currentIndex+1) max maxIndex

construccio :: Dataset -> DecisionTree
construccio dataset
  | infoGainMax == 0 = Leaf "leaf"
  | otherwise = Node attrName fills
    where 
      infoGainMax = fst (getMaxWithIndex $ infoGainList dataset)
      infoIndex = snd (getMaxWithIndex $ infoGainList dataset)
      headers = fst dataset
      matrix = snd dataset
      attrName = fst (headers !! infoIndex)
      fills = map (\value -> (value, construccio (headers, filteredRows infoIndex attrName matrix))) (snd (headers !! infoIndex))
    



-- MAIN
{- main :: IO ()
main = do
    contents <- readFile "agaricus-lepiota.data"
    mapM_ print $ map  (removeChar ",") $ lines contents
 -}

{- gini :: Dataset -> [Float]
gini data =  -}

main :: IO()
main = print table

main' = ["pxnk","exyk","ebwn","pxwn","exyn","ebwn","pxwp"]

parse :: [Char] -> Row
parse s = parse' s 0 []

parse' :: [Char] -> Int -> [AttValue] -> Row
parse' s index acc
  | null s = reverse acc
  | otherwise = parse' (tail s) (index+1) (parse'' (head s) index:acc)

parse'' :: Char -> Int -> AttValue
parse'' char index
  | index == 0 = charToClassification char
  | index == 1 = charToCapShape char
  | index == 2 = charToCapColor char
  | index == 3 = charToGillColor char
  | otherwise = error $ unwords ["Unexpected feature value :", show index]

-- DATA MODELS
mushrooms :: Dataset
mushrooms = (header, table)

header :: [Attribute]
header = [classification, capShape, capColor, gillColor]
table :: [Row]
table =  map parse main'


classification :: Attribute
classification = ("class", ["edible","poisonous"])

capShape :: Attribute
capShape = ("cap-shape", ["bell" ,"conical" ,"convex" ,"flat" ,"knobbed" ,"sunken"])


capColor :: Attribute
capColor = ("cap-color", ["brown","buff","cinnamon","gray","green","pink","purple","red","white","yellow"])

gillColor :: Attribute
gillColor = ("gill-color", ["black","brown","buff","chocolate","gray", "green","orange","pink","purple","red", "white","yellow"])

type AttName = String
type AttValue = String
type Attribute = (AttName, [AttValue])
type Header = [Attribute]
type Row = [AttValue]
type Dataset = (Header, [Row])

data DecisionTree = Leaf AttValue | Node AttName [(AttValue, DecisionTree)] deriving (Eq, Show)

type Dict = (String -> (Int, Float))
create = const
search = ($)
insert dict key value x
  | key == x  = value
  | otherwise = dict x

charToClassification :: Char -> String
charToClassification c 
  | c == 'p' = snd classification !! 1
  | c == 'e' = snd classification !! 0
  | otherwise = error $ unwords ["Unexpected feature value :", show c]

charToCapShape :: Char -> String
charToCapShape c
 | c == 'b' = snd capShape !! 0
 | c == 'c' = snd capShape !! 1
 | c == 'x' = snd capShape !! 2
 | c == 'f' = snd capShape !! 3
 | c == 'k' = snd capShape !! 4
 | c == 's' = snd capShape !! 5
 |otherwise = error $ unwords ["Unexpected feature value :", show c]

data CapSurface = CSFibrous | CSGrooves | CSScaly | CSSmooth deriving (Eq, Read, Show, Ord, Enum )
charToCapSurface :: Char -> CapSurface
charToCapSurface c
 | c == 'f' = CSFibrous
 | c == 'g' = CSGrooves
 | c == 'y' = CSScaly
 | c == 's' = CSSmooth
  |otherwise = error $ unwords ["Unexpected feature value :", show c]


charToCapColor :: Char -> String
charToCapColor c
 | c == 'n' = snd capColor !! 0
 | c == 'b' = snd capColor !! 1
 | c == 'c' = snd capColor !! 2
 | c == 'g' = snd capColor !! 3
 | c == 'r' = snd capColor !! 4
 | c == 'p' = snd capColor !! 5
 | c == 'u' = snd capColor !! 6
 | c == 'e' = snd capColor !! 7
 | c == 'w' = snd capColor !! 8
 | c == 'y' = snd capColor !! 9
  |otherwise = error $ unwords ["Unexpected feature value :", show c]

charToBruises :: Char -> Bool
charToBruises c 
 | c == 't' = True
 | c == 'f' = False
  |otherwise = error $ unwords ["Unexpected feature value :", show c]

data Odor = Almond | Anise | Creosote | Fishy | Foul | Musty | None | Pungent | Spicy deriving (Eq, Read, Show, Ord, Enum )
charToOdor :: Char -> Odor
charToOdor c
 | c == 'a' = Almond
 | c == 'l' = Anise
 | c == 'c' = Creosote
 | c == 'y' = Fishy
 | c == 'f' = Foul
 | c == 'm' = Musty
 | c == 'n' = None
 | c == 'p' = Pungent
 | c == 's' = Spicy
  |otherwise = error $ unwords ["Unexpected feature value :", show c]

data GillAttachment = Attached | Descending | Free | Notched deriving (Eq, Read, Show, Ord, Enum )
charToGillAttachment :: Char -> GillAttachment
charToGillAttachment c
 | c == 'a' = Attached
 | c == 'd' = Descending
 | c == 'f' = Free
 | c == 'n' = Notched
  |otherwise = error $ unwords ["Unexpected feature value :", show c]

data GillSpacing = Close | Crowded | Distant deriving (Eq, Read, Show, Ord, Enum )
charToGillSpacing :: Char -> GillSpacing
charToGillSpacing c
 | c == 'c' = Close
 | c == 'w' = Crowded
 | c == 'd' = Distant
  |otherwise = error $ unwords ["Unexpected feature value :", show c]

data GillSize = Broad | Narrow deriving (Eq, Read, Show, Ord, Enum )
charToGillSize :: Char -> GillSize
charToGillSize c
 | c == 'b' = Broad
 | c == 'n' = Narrow
  |otherwise = error $ unwords ["Unexpected feature value :", show c]

data GillColor = GCBlack | GCBrown | GCBuff | GCChocolate | GCGray | GCGreen | GCOrange | GCPink | GCPurple | GCRed | GCWhite | GCYellow deriving (Eq, Read, Show, Ord, Enum )
charToGillColor :: Char -> String
charToGillColor c
 | c == 'k' = snd gillColor !! 0
 | c == 'n' = snd gillColor !! 1
 | c == 'b' = snd gillColor !! 2
 | c == 'h' = snd gillColor !! 3
 | c == 'g' = snd gillColor !! 4
 | c == 'r' = snd gillColor !! 5
 | c == 'o' = snd gillColor !! 6
 | c == 'p' = snd gillColor !! 7
 | c == 'u' = snd gillColor !! 8
 | c == 'e' = snd gillColor !! 9
 | c == 'w' = snd gillColor !! 10
  |otherwise = error $ unwords ["Unexpected feature value :", show c]

data StalkShape = Enlarging | Tapering deriving (Eq, Read, Show, Ord, Enum )
charToStalkShape :: Char -> StalkShape
charToStalkShape c
 | c == 'e' = Enlarging
 | c == 't' = Tapering
  |otherwise = error $ unwords ["Unexpected feature value :", show c]

data StalkRoot = Bulbous | Club | Cup | Equal | Rhizomorphs | Rooted deriving (Eq, Read, Show, Ord, Enum )

charToStalkRoot :: Char -> Maybe StalkRoot
charToStalkRoot c
 | c == 'b' = Just Bulbous
 | c == 'c' = Just Club
 | c == 'u' = Just Cup
 | c == 'e' = Just Equal
 | c == 'z' = Just Rhizomorphs
 | c == 'r' = Just Rooted
 | c == '?'= Nothing
  |otherwise = error $ unwords ["Unexpected feature value :", show c]

data StalkSurfaceAboveRing = SSARFibrous | SSARScaly | SSARSilky | SSARSmooth deriving (Eq, Read, Show, Ord, Enum )
charToSsar :: Char -> StalkSurfaceAboveRing
charToSsar c
 | c == 'f' = SSARFibrous
 | c == 'y' = SSARScaly
 | c == 'k' = SSARSilky
 | c == 's' = SSARSmooth
  |otherwise = error $ unwords ["Unexpected feature value :", show c]

data StalkSurfaceBelowRing = SSBRFibrous | SSBRScaly | SSBRSilky | SSBRSmooth deriving (Eq, Read, Show, Ord, Enum )
charToSsbr :: Char -> StalkSurfaceBelowRing
charToSsbr c
 | c == 'f' = SSBRFibrous
 | c == 'y' = SSBRScaly
 | c == 'k' = SSBRSilky
 | c == 's' = SSBRSmooth
  |otherwise = error $ unwords ["Unexpected feature value :", show c]

data StalkColorAboveRing = SCARBrown | SCARBuff | SCARCinnamon | SCARGray | SCAROrange | SCARPink | SCARRed | SCARWhite | SCARYellow deriving (Eq, Read, Show, Ord, Enum )
charToScar :: Char -> StalkColorAboveRing
charToScar c
 | c == 'n' = SCARBrown
 | c == 'b' = SCARBuff
 | c == 'c' = SCARCinnamon
 | c == 'g' = SCARGray
 | c == 'o' = SCAROrange
 | c == 'p' = SCARPink
 | c == 'e' = SCARRed
 | c == 'w' = SCARWhite
 | c == 'y' = SCARYellow
  |otherwise = error $ unwords ["Unexpected feature value :", show c]

data StalkColorBelowRing = SCBRBrown | SCBRBuff | SCBRCinnamon | SCBRGray | SCBROrange | SCBRPink | SCBRRed | SCBRWhite | SCBRYellow deriving (Eq, Read, Show, Ord, Enum )
charToScbr :: Char -> StalkColorBelowRing
charToScbr c
 | c == 'n' = SCBRBrown
 | c == 'b' = SCBRBuff
 | c == 'c' = SCBRCinnamon
 | c == 'g' = SCBRGray
 | c == 'o' = SCBROrange
 | c == 'p' = SCBRPink
 | c == 'e' = SCBRRed
 | c == 'w' = SCBRWhite
 | c == 'y' = SCBRYellow
  |otherwise = error $ unwords ["Unexpected feature value :", show c]

data VeilType = Partial | Universal deriving (Eq, Read, Show, Ord, Enum )
charToVeilType :: Char -> VeilType
charToVeilType c
 | c == 'p' = Partial
 | c == 'u' = Universal
  |otherwise = error $ unwords ["Unexpected feature value :", show c]

data VeilColor = VCBrown | VCOrange | VCWhite | VCYellow deriving (Eq, Read, Show, Ord, Enum )
charToVeilColor :: Char -> VeilColor
charToVeilColor c
 | c == 'n' = VCBrown
 | c == 'o' = VCOrange
 | c == 'w' = VCWhite
 | c == 'y' = VCYellow
  |otherwise = error $ unwords ["Unexpected feature value :", show c]

-- | 18. ring-number: none=n,one=o,two=t
data RingNumber = RNNone | RNOne | RNTwo deriving (Eq, Read, Show, Ord, Enum )
charToRingNumber :: Char -> RingNumber
charToRingNumber c
 | c == 'n' = RNNone
 | c == 'o' = RNOne
 | c == 't' = RNTwo
  |otherwise = error $ unwords ["Unexpected feature value :", show c]

data RingType = RTCobwebby | RTEvanescent | RTFlaring | RTLarge | RTNone | RTPendant | RTSheathing | RTZone deriving (Eq, Read, Show, Ord, Enum )
charToRingType :: Char -> RingType
charToRingType c
 | c == 'c' = RTCobwebby
 | c == 'e' = RTEvanescent
 | c == 'f' = RTFlaring
 | c == 'l' = RTLarge
 | c == 'n' = RTNone
 | c == 'p' = RTPendant
 | c == 's' = RTSheathing
 | c == 'z' = RTZone
  |otherwise = error $ unwords ["Unexpected feature value :", show c]

data SporePrintColor = SPCBlack | SPCBrown | SPCBuff | SPCChocolate | SPCGreen | SPCOrange | SPCPurple | SPCWhite | SPCYellow deriving (Eq, Read, Show, Ord, Enum )
charToSporePrintColor :: Char -> SporePrintColor
charToSporePrintColor c
 | c == 'k' = SPCBlack
 | c == 'n' = SPCBrown
 | c == 'b' = SPCBuff
 | c == 'h' = SPCChocolate
 | c == 'r' = SPCGreen
 | c == 'o' = SPCOrange
 | c == 'u' = SPCPurple
 | c == 'w' = SPCWhite
 | c == 'y' = SPCYellow
  |otherwise = error $ unwords ["Unexpected feature value :", show c]

data Population = Abundant | Clustered | Numerous | Scattered | Several | Solitary deriving (Eq, Read, Show, Ord, Enum )
charToPopulation :: Char -> Population
charToPopulation c
 | c == 'a' = Abundant
 | c == 'c' = Clustered
 | c == 'n' = Numerous
 | c == 's' = Scattered
 | c == 'v' = Several
 | c == 'y' = Solitary
  |otherwise = error $ unwords ["Unexpected feature value :", show c]

data Habitat = Grasses | Leaves | Meadows | Paths | Urban | Waste | Woods deriving (Eq, Read, Show, Ord, Enum )
charToHabitat :: Char -> Habitat
charToHabitat c
 | c == 'g' = Grasses
 | c == 'l' = Leaves
 | c == 'm' = Meadows
 | c == 'p' = Paths
 | c == 'u' = Urban
 | c == 'w' = Waste
 | c == 'd' = Woods
  |otherwise = error $ unwords ["Unexpected feature value :", show c]