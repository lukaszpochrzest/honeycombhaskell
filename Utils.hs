module Utils where

-- |Checks if Maybe has any result
isJust (Just _) = True
isJust _        = False

-- |Extracts Maybe result
unJust (Just a) = a

-- |Creates list by removing element form an old list
removeAt [] _ = []
removeAt xs n = fst ys ++ (tail (snd ys))
                 where ys = splitAt n xs

-- TODO remove this one, its not used really
insertAt newElem xs n = fst ys ++ [newElem] ++ (tail (snd ys))
                 where ys = splitAt n xs

removeThanInsertAt e list atIndex = removeThanInsertAtInternal e list atIndex 0

removeThanInsertAtInternal e (x:xs) atIndex currIndex | atIndex == currIndex = e:xs
                                                      | otherwise            = x:(removeThanInsertAtInternal e xs atIndex (currIndex+1))

-- | Returns intersection if two lists
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

-- | Returns true if there are any duplicates in the list, false otherwise
duplicates [] = False
duplicates (v:vs) | elem v vs = True
                  | otherwise = duplicates vs
-- TESTS:
-- duplicates [] == False
-- duplicates [1,2,3,4,3] == True
-- duplicates [1,2,3,4,5,6] == False

