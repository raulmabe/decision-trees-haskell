-- UTILITIES
   
{- CALCULATES THE HEIGHT OF THE TREE -}
height :: DecisionTree -> Int
height Null = 0
height (Leaf _) = 1
height (Node _ fills) = 1 +  maximum ( map (height . snd) fills)

{- TRANSPOSES A MATRIX -}
transpose               :: [[a]] -> [[a]]
transpose []             = []
transpose ([]   : xss)   = transpose xss
transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])

{- REMOVES ALL OCURRENCES OF A STRING IN A STRING -}
removeChar :: String -> String -> String
removeChar c  = filter (not . (`elem` c)) 

{- REMOVES DUPLICATES OF A STRING -}
nub :: Eq a => [a] -> [a]
nub (x:xs) = x : nub (filter (/= x) xs)
nub [] = []

{- ENUMERA QUANTS COPS SURT UNA MATEIXA STRING EN UNA LLISTA DE STRINGS -}
classCounts :: Row -> [(String,Int)]
classCounts row = map (`classCounts'` row) $ nub row

classCounts' :: String -> Row -> (String,Int)
classCounts' a row = (a, length $ filter (== a) row)

{- CALC THE GINI IMPURITY -}
gini :: Row -> Float
gini row = gini' (length row)  (classCounts row) 1 

gini' :: Int -> [(String,Int)] -> Float  -> Float
gini' length appears impurity 
  | null appears = impurity
  | otherwise = gini' length (tail appears) $ impurity - ((fromIntegral (snd (head appears)) / fromIntegral length) ** 2.0)

{- CALC THE INFO GAIN OF EACH POSSIBLE COLUMN OF A DATASET -}
infoGainList :: Dataset -> [Float]
infoGainList (header, matrix) = map calcInfoGain $ tail $ zip [0..] header
  where calcInfoGain attrWithIndex = infoGain attrWithIndex matrix

infoGain :: (Int, Attribute) -> [Row] -> Float
infoGain _ [[]] = 0.0
infoGain _ [] = 0.0
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

{- REDUCE MATRIX TO ONES CONCORDING WITH KEY -}
filteredRows :: Int -> String -> [Row] -> [Row]
filteredRows index key  = filter (\row -> row !! index == key) 

{- Get max value of an array and its index -}
getMaxWithIndex :: [Float] -> (Float, Int)
getMaxWithIndex xs = getMaxWithIndex' xs 0 0 0

getMaxWithIndex' :: [Float] -> Int -> Float -> Int -> (Float,Int)
getMaxWithIndex' llista currentIndex max maxIndex
  | length llista == currentIndex = (max,maxIndex)
  | (llista !! currentIndex) > max = getMaxWithIndex' llista (currentIndex+1) (llista !! currentIndex) currentIndex
  | otherwise = getMaxWithIndex' llista (currentIndex+1) max maxIndex

{- Build a decision tree based on a dataset -}
construccio :: Dataset -> DecisionTree
construccio dataset
  | null $ snd dataset = Null
  | infoGainMax == 0.0 = Leaf $ head $ head $ snd dataset
  | otherwise = Node attrName fills
    where 
      infoGainMax = fst (getMaxWithIndex $ infoGainList dataset)
      infoIndex = snd (getMaxWithIndex $ infoGainList dataset) +1
      headers = fst dataset
      matrix = snd dataset
      attrName = fst (headers !! infoIndex)
      fills = map (\value -> (value, construccio (headers, filteredRows infoIndex value matrix))) (snd (headers !! infoIndex))

{- Prints tree -}
printTree :: DecisionTree -> IO()
printTree tree = putStrLn $ drawTree tree 0

{- Draws tree in string -}
drawTree :: DecisionTree -> Int -> String
drawTree Null _ =  []
drawTree (Leaf value) level = concat (replicate level "  " ) ++ value 
drawTree (Node name fills) level = concat (replicate level "  " ) ++ name ++ "\n" ++ concatMap (drawTree' (level +1)) fills
  

drawTree' :: Int -> (AttValue, DecisionTree)  -> String
drawTree' _ (_,Null) = []
drawTree' level (value,tree) = concat (replicate level "  " ) ++ value ++ "\n" ++ drawTree tree (level+1)  ++ "\n"


main :: IO()
main = do
  contents <- readFile "agaricus-lepiota.data"
  let dades = map  (removeChar ",") $ lines contents
  let table = map parse dades
  let mushrooms = (header, table)
  
  putStrLn "--- PRETTY DECISION TREE ---"
  printTree $ construccio mushrooms
  putStrLn "--- CLASSIFICATION ---"
  classificate $ construccio mushrooms
  
classificate :: DecisionTree -> IO()
classificate Null = return ()
classificate (Leaf value) = putStrLn $ "Prediction: " ++ value
classificate (Node name fills) = do
  putStrLn $ "Which " ++ name ++ "?"
  i <- getLine
  if elem i $ map fst fills then do
    classificate $ snd $ head $ filter (\(value, _) -> value == i) fills 
  else
    classificate (Node name fills)

{- Parsing methods -}
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
  | index == 2 = charToCapSurface char
  | index == 3 = charToCapColor char
  | index == 4 = charToBruises char
  | index == 5 = charToOdor char
  | index == 6 = charToGillAttachment char
  | index == 7 = charToGillSpacing char
  | index == 8 = charToGillSize char
  | index == 9 = charToGillColor char
  | index == 10 = charToStalkShape char
  | index == 11 = charToStalkRoot char
  | index == 12 = charToSsar char
  | index == 13 = charToSsbr char
  | index == 14 = charToScar char
  | index == 15 = charToScbr char
  | index == 16 = charToVeilType char
  | index == 17 = charToVeilColor char
  | index == 18 = charToRingNumber char
  | index == 19 = charToRingType char
  | index == 20 = charToSporePrintColor char
  | index == 21 = charToPopulation char
  | index == 22 = charToHabitat char
  | otherwise = error $ unwords ["Unexpected feature value =:", show index]

-- DATA MODELS
{- http://hackage.haskell.org/package/datasets-0.3.0/docs/src/Numeric.Datasets.Mushroom.html#MushroomEntry -}

{- 
MINOR DATA FOR TESTING
minorData = ["pxnk","exyk","ebwn","pxwn","exyn","ebwn","pxwp"]
mushrooms :: Dataset
mushrooms = (header, table)
table :: [Row]
table =  map parse minorData -}

header :: [Attribute]
header = [
          classification, capShape, capSurface, capColor, bruises, odor, gillAttachment, gillSpacing, gillSize, 
          gillColor, stalkShape, stalkRoot, stalkSurfaceAboveRing, stalkSurfaceBelowRing, stalkColorAboveRing , stalkColorBelowRing, 
          veilType, veilColor, ringNumber, ringType, sporePrintColor, population, habitat
          ]

type AttName = String
type AttValue = String
type Attribute = (AttName, [AttValue])
type Header = [Attribute]
type Row = [AttValue]
type Dataset = (Header, [Row])

data DecisionTree = Null | Leaf AttValue | Node AttName [(AttValue, DecisionTree)] deriving (Eq, Show)

type Dict = (String -> (Int, Float))
create = const
search = ($)
insert dict key value x
  | key == x  = value
  | otherwise = dict x

classification :: Attribute
classification = ("class", ["edible","poisonous"])
charToClassification :: Char -> String
charToClassification c 
  | c == 'p' = snd classification !! 1
  | c == 'e' = snd classification !! 0
  | otherwise = error $ unwords ["Unexpected feature value classification:", show c]


capShape :: Attribute
capShape = ("cap-shape", ["bell" ,"conical" ,"convex" ,"flat" ,"knobbed" ,"sunken"])
charToCapShape :: Char -> String
charToCapShape c
 | c == 'b' = snd capShape !! 0
 | c == 'c' = snd capShape !! 1
 | c == 'x' = snd capShape !! 2
 | c == 'f' = snd capShape !! 3
 | c == 'k' = snd capShape !! 4
 | c == 's' = snd capShape !! 5
 |otherwise = error $ unwords ["Unexpected feature value capShape:", show c]

capSurface :: Attribute
capSurface = ("cap-surface", ["fibrous","grooves","scaly","smooth"])
charToCapSurface :: Char -> String
charToCapSurface c
 | c == 'f' = snd capSurface !! 0
 | c == 'g' = snd capSurface !! 1
 | c == 'y' = snd capSurface !! 2
 | c == 's' = snd capSurface !! 3
  |otherwise = error $ unwords ["Unexpected feature value capSurface:", show c]



capColor :: Attribute
capColor = ("cap-color", ["brown","buff","cinnamon","gray","green","pink","purple","red","white","yellow"])
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
  |otherwise = error $ unwords ["Unexpected feature value capColor:", show c]

bruises :: Attribute
bruises = ("bruises", ["true", "false"])
charToBruises :: Char -> String
charToBruises c 
 | c == 't' = snd bruises !! 0
 | c == 'f' = snd bruises !! 1
  |otherwise = error $ unwords ["Unexpected feature value bruises:", show c]

odor :: Attribute
odor = ("odor", ["almond","anise","creosote","fishy","foul", "musty","none","pungent","spicy"])
charToOdor :: Char -> String
charToOdor c
 | c == 'a' = snd odor !! 0
 | c == 'l' = snd odor !! 1
 | c == 'c' = snd odor !! 2
 | c == 'y' = snd odor !! 3
 | c == 'f' = snd odor !! 4
 | c == 'm' = snd odor !! 5
 | c == 'n' = snd odor !! 6
 | c == 'p' = snd odor !! 7
 | c == 's' = snd odor !! 8
  |otherwise = error $ unwords ["Unexpected feature value odor:", show c]

gillAttachment :: Attribute
gillAttachment = ("gill-attachment",["attached","descending","free","notched"])
charToGillAttachment :: Char -> String
charToGillAttachment c
 | c == 'a' = snd gillAttachment !! 0
 | c == 'd' = snd gillAttachment !! 1
 | c == 'f' = snd gillAttachment !! 2
 | c == 'n' = snd gillAttachment !! 3
  |otherwise = error $ unwords ["Unexpected feature value gillAttachment:", show c]

gillSpacing :: Attribute
gillSpacing = ("gill-spacing", ["close","crowded","distant"])
charToGillSpacing :: Char -> String
charToGillSpacing c
 | c == 'c' = snd gillSpacing !! 0
 | c == 'w' = snd gillSpacing !! 1
 | c == 'd' = snd gillSpacing !! 2
  |otherwise = error $ unwords ["Unexpected feature value gillSpacing:", show c]

gillSize :: Attribute
gillSize = ("gill-size", ["broad","narrow"])
charToGillSize :: Char -> String
charToGillSize c
 | c == 'b' = snd gillSize !! 0
 | c == 'n' = snd gillSize !! 1
  |otherwise = error $ unwords ["Unexpected feature value gillSize:", show c]


gillColor :: Attribute
gillColor = ("gill-color", ["black","brown","buff","chocolate","gray", "green","orange","pink","purple","red", "white","yellow"])
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
 | c == 'y' = snd gillColor !! 11
  |otherwise = error $ unwords ["Unexpected feature value gillColor:", show c]

stalkShape :: Attribute
stalkShape = ("stalk-shape", ["enlarging","tapering"])
charToStalkShape :: Char -> String
charToStalkShape c
 | c == 'e' = snd stalkShape !! 0
 | c == 't' = snd stalkShape !! 1
  |otherwise = error $ unwords ["Unexpected feature value stalkShape:", show c]

stalkRoot :: Attribute
stalkRoot = ("stalk-root", ["bulbous","club","cup","equal", "rhizomorphs","rooted","missing"])
charToStalkRoot :: Char -> String
charToStalkRoot c
 | c == 'b' = snd stalkRoot !! 0
 | c == 'c' = snd stalkRoot !! 1
 | c == 'u' = snd stalkRoot !! 2
 | c == 'e' = snd stalkRoot !! 3
 | c == 'z' = snd stalkRoot !! 4
 | c == 'r' = snd stalkRoot !! 5
 | c == '?' = snd stalkRoot !! 6
  |otherwise = error $ unwords ["Unexpected feature value stalkRoot:", show c]

stalkSurfaceAboveRing :: Attribute
stalkSurfaceAboveRing = ("stalk-surface-above-ring", ["fibrous","scaly","silky","smooth"]) 
charToSsar :: Char -> String
charToSsar c
 | c == 'f' = snd stalkSurfaceAboveRing !! 0
 | c == 'y' = snd stalkSurfaceAboveRing !! 1
 | c == 'k' = snd stalkSurfaceAboveRing !! 2
 | c == 's' = snd stalkSurfaceAboveRing !! 3
  |otherwise = error $ unwords ["Unexpected feature value stalkSurfaceAboveRing:", show c]

stalkSurfaceBelowRing :: Attribute
stalkSurfaceBelowRing = ("stalk-surface-below-ring", ["fibrous","scaly","silky","smooth"])
charToSsbr :: Char -> String
charToSsbr c
 | c == 'f' = snd stalkSurfaceBelowRing !! 0
 | c == 'y' = snd stalkSurfaceBelowRing !! 1
 | c == 'k' = snd stalkSurfaceBelowRing !! 2
 | c == 's' = snd stalkSurfaceBelowRing !! 3
  |otherwise = error $ unwords ["Unexpected feature value stalkSurfaceBelowRing:", show c]

stalkColorAboveRing :: Attribute
stalkColorAboveRing = ("stalk-color-above-ring", ["brown","buff","cinnamon","gray","orange", "pink","red","white","yellow"])
charToScar :: Char -> String
charToScar c
 | c == 'n' = snd stalkColorAboveRing !! 0
 | c == 'b' = snd stalkColorAboveRing !! 1
 | c == 'c' = snd stalkColorAboveRing !! 2
 | c == 'g' = snd stalkColorAboveRing !! 3
 | c == 'o' = snd stalkColorAboveRing !! 4
 | c == 'p' = snd stalkColorAboveRing !! 5
 | c == 'e' = snd stalkColorAboveRing !! 6
 | c == 'w' = snd stalkColorAboveRing !! 7
 | c == 'y' = snd stalkColorAboveRing !! 8
  |otherwise = error $ unwords ["Unexpected feature value stalkColorAboveRing:", show c]

stalkColorBelowRing = ( "stalk-color-below-ring",["brown","buff","cinnamon","gray","orange", "pink","red","white","yellow"])
charToScbr :: Char -> String
charToScbr c
 | c == 'n' = snd stalkColorBelowRing !! 0
 | c == 'b' = snd stalkColorBelowRing !! 1
 | c == 'c' = snd stalkColorBelowRing !! 2
 | c == 'g' = snd stalkColorBelowRing !! 3
 | c == 'o' = snd stalkColorBelowRing !! 4
 | c == 'p' = snd stalkColorBelowRing !! 5
 | c == 'e' = snd stalkColorBelowRing !! 6
 | c == 'w' = snd stalkColorBelowRing !! 7
 | c == 'y' = snd stalkColorBelowRing !! 8
  |otherwise = error $ unwords ["Unexpected feature value stalkColorBelowRing:", show c]

veilType :: Attribute
veilType = ("veil-type", ["partial","universal"])
charToVeilType :: Char -> String
charToVeilType c
 | c == 'p' = snd veilType !! 0
 | c == 'u' = snd veilType !! 1
  |otherwise = error $ unwords ["Unexpected feature value veilType:", show c]

veilColor :: Attribute
veilColor = ("veil-color",["brown","orange","white","yellow"])
charToVeilColor :: Char -> String
charToVeilColor c
 | c == 'n' = snd veilColor !! 0
 | c == 'o' = snd veilColor !! 1
 | c == 'w' = snd veilColor !! 2
 | c == 'y' = snd veilColor !! 3
  |otherwise = error $ unwords ["Unexpected feature value veilColor:", show c]

ringNumber :: Attribute
ringNumber = ("ring-number", ["none","one","two"])
charToRingNumber :: Char -> String
charToRingNumber c
 | c == 'n' = snd ringNumber !! 0
 | c == 'o' = snd ringNumber !! 1
 | c == 't' = snd ringNumber !! 2
  |otherwise = error $ unwords ["Unexpected feature value ringNumber:", show c]

ringType :: Attribute
ringType = ("ring-type", ["cobwebby","evanescent","flaring","large", "none","pendant","sheathing","zone"])
charToRingType :: Char -> String
charToRingType c
 | c == 'c' = snd ringType !! 0
 | c == 'e' = snd ringType !! 1
 | c == 'f' = snd ringType !! 2
 | c == 'l' = snd ringType !! 3
 | c == 'n' = snd ringType !! 4
 | c == 'p' = snd ringType !! 5
 | c == 's' = snd ringType !! 6
 | c == 'z' = snd ringType !! 7
  |otherwise = error $ unwords ["Unexpected feature value ringType:", show c]

sporePrintColor :: Attribute
sporePrintColor = ("spore-print-color",["black","brown","buff","chocolate","green", "orange","purple","white","yellow"])
charToSporePrintColor :: Char -> String
charToSporePrintColor c
 | c == 'k' = snd sporePrintColor !! 0
 | c == 'n' = snd sporePrintColor !! 1
 | c == 'b' = snd sporePrintColor !! 2
 | c == 'h' = snd sporePrintColor !! 3
 | c == 'r' = snd sporePrintColor !! 4
 | c == 'o' = snd sporePrintColor !! 5
 | c == 'u' = snd sporePrintColor !! 6
 | c == 'w' = snd sporePrintColor !! 7
 | c == 'y' = snd sporePrintColor !! 8
  |otherwise = error $ unwords ["Unexpected feature value sporePrintColor:", show c]

population :: Attribute
population = ("population", ["abundant","clustered","numerous", "scattered","several","solitary"])
charToPopulation :: Char -> String
charToPopulation c
 | c == 'a' = snd population !! 0
 | c == 'c' = snd population !! 1
 | c == 'n' = snd population !! 2
 | c == 's' = snd population !! 3
 | c == 'v' = snd population !! 4
 | c == 'y' = snd population !! 5
  |otherwise = error $ unwords ["Unexpected feature value population:", show c]

habitat :: Attribute
habitat = ("habitat", ["grasses","leaves","meadows","paths", "urban","waste","woods"])
charToHabitat :: Char -> String
charToHabitat c
 | c == 'g' = snd habitat !! 0
 | c == 'l' = snd habitat !! 1
 | c == 'm' = snd habitat !! 2
 | c == 'p' = snd habitat !! 3
 | c == 'u' = snd habitat !! 4
 | c == 'w' = snd habitat !! 5
 | c == 'd' = snd habitat !! 6
  |otherwise = error $ unwords ["Unexpected feature value habitat:", show c]