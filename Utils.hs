module Utils where

-- | Checks if Maybe has any result
isJust (Just _) = True
isJust _        = False

-- | Extracts Maybe result
unJust (Just a) = a

-- | Creates list by removing element form an old list
removeAt [] _ = []
removeAt xs n = fst ys ++ (tail (snd ys))
                 where ys = splitAt n xs

-- | Swaps element in matrix at given coordinates
swapInMatrixAt e matrix x y = swapInMatrixAtInternal e matrix x y 0

swapInMatrixAtInternal e (r:rs) x y currY | y == currY = (swapAt e r x):rs
                                          | otherwise      = r:(swapInMatrixAtInternal e rs x y (currY+1))

-- | Swaps element in list at given index
swapAt e list atIndex = swapAtInternal e list atIndex 0

swapAtInternal e (x:xs) atIndex currIndex | atIndex == currIndex = e:xs
                                          | otherwise            = x:(swapAtInternal e xs atIndex (currIndex+1))


-- | Evaluates to intersection if two lists
intersection [] _            = []
intersection _ []            = []
intersection (c1:c1s) c2List = if elem c1 c2List
                                then c1:(intersection c1s c2List)
                                else intersection c1s c2List 
-- TESTS:
-- intersection [1,2,3,4,5] [0,1,2,5,8] == [1,2,5]
-- intersection [0] [] == []
-- intersection [] [1] == []
-- intersection [0,1,2] [3,4,5,6] == []

-- | Evaluates to true if there are any duplicates in the list, false otherwise
duplicates [] = False
duplicates (v:vs) | elem v vs = True
                  | otherwise = duplicates vs
-- TESTS:
-- duplicates [] == False
-- duplicates [1,2,3,4,3] == True
-- duplicates [1,2,3,4,5,6] == False

-- | Finds element in matrix that satisfies given predicate
findInMatrix [] _ = Nothing
findInMatrix (row:rows) recognise = if found /= Nothing then found else findInMatrix rows recognise
                                        where found = findInRow row recognise

-- | Finds element in list that satisfies given predicate
findInRow [] _= Nothing
findInRow (h:hs) recognise = if recognise h then Just h else findInRow hs recognise

-- | Maps matrix using given function
mapMatrix f [] _     = []
mapMatrix f (r:rs) y = (mapRow f r 0 y):(mapMatrix f rs (y+1))

-- | Maps list using given function
mapRow f [] _ _        = []
mapRow f (e:es) x y = (f e x y):(mapRow f es (x+1) y)
