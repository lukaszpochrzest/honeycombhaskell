module Utils where

-- HELPERS
isJust (Just _) = True
isJust _        = False

unJust (Just a) = a

removeAt xs n = fst ys ++ (tail (snd ys))
                 where ys = splitAt n xs

insertAt newElem xs n = fst ys ++ [newElem] ++ (tail (snd ys))
                 where ys = splitAt n xs

-- TODO comment
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

-- TODO comment
duplicates [] = False
duplicates (v:vs) | elem v vs = True
                  | otherwise = duplicates vs
-- TESTS:
-- duplicates [] == False
-- duplicates [1,2,3,4,3] == True
-- duplicates [1,2,3,4,5,6] == False

