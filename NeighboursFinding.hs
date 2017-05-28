module NeighboursFinding (neighbours, neighboursNeighbours) where
import Model
import Utils


-- | Finds neighbours of given Hex
neighbours hex state = map unJust (filter isJust ((getNENeighbour hex state):(getENeighbour hex state):(getSENeighbour hex state):(getSWNeighbour hex state):(getWNeighbour hex state):(getNWNeighbour hex state):[]))

getNENeighbour (Hex val row column) state | isTopRow (Hex val row column) state = Nothing
                                          | (isLastColumn (Hex val row column) state) && odd row = Nothing
                                          | odd row = Just (getHex state (row - 1) column)
                                          | even row = Just (getHex state (row - 1) (column + 1))
                                          | otherwise = error "what the heck"

getNWNeighbour (Hex val row column) state | isTopRow (Hex val row column) state = Nothing
                                          | (isFirstColumn (Hex val row column) state) && odd row = Nothing
                                          | odd row = Just (getHex state (row - 1) (column - 1))
                                          | even row = Just (getHex state (row - 1) column)
                                          | otherwise = error "what the heck"

getSENeighbour (Hex val row column) state | isLastRow (Hex val row column) state = Nothing
                                          | (isLastColumn (Hex val row column) state) && odd row = Nothing
                                          | odd row = Just (getHex state (row + 1) column)
                                          | even row = Just (getHex state (row + 1) (column + 1))
                                          | otherwise = error "what the heck"

getSWNeighbour (Hex val row column) state | isLastRow (Hex val row column) state = Nothing
                                          | (isFirstColumn (Hex val row column) state) && odd row = Nothing
                                          | odd row = Just (getHex state (row + 1) (column - 1))
                                          | even row = Just (getHex state (row + 1) (column))
                                          | otherwise = error "what the heck"

getENeighbour (Hex val row column) state  | isLastColumn (Hex val row column) state = Nothing
                                          | otherwise = Just (getHex state row (column + 1))

getWNeighbour (Hex val row column) state  | isFirstColumn (Hex val row column) state = Nothing
                                          | otherwise = Just (getHex state row (column - 1))

-- neighbours (Hex B 1 0) (State [[Hex A 0 0], [Hex B 1 0, Hex C 1 1]] 2)
--
-- neighbours (Hex A 0 0) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex D 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == [Hex B 0 1,Hex D 1 1,Hex C 1 0]
-- neighbours (Hex B 0 1) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex D 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == [Hex E 1 2,Hex D 1 1,Hex A 0 0]
-- neighbours (Hex C 1 0) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex D 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == [Hex A 0 0,Hex D 1 1,Hex F 2 0]
-- neighbours (Hex D 1 1) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex D 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == [Hex B 0 1,Hex E 1 2,Hex G 2 1,Hex F 2 0,Hex C 1 0,Hex A 0 0]
-- neighbours (Hex E 1 2) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex D 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == [Hex G 2 1,Hex D 1 1,Hex B 0 1]
-- neighbours (Hex F 2 0) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex D 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == [Hex D 1 1,Hex G 2 1,Hex C 1 0]
-- neighbours (Hex G 2 1) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex D 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == [Hex E 1 2,Hex F 2 0,Hex D 1 1]

-- https://stackoverflow.com/questions/40025319/fmap-and-flat-map-in-haskell
-- | Finds neighbours of neighbours of given Hex
neighboursNeighbours hex state = (neighbours hex state) >>= \neighbour -> neighbours neighbour state

-- neighboursNeighbours (Hex A 0 0) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex Empty 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3)
-- neighboursNeighbours (Hex A 0 0) (State [[Hex A 0 0, Hex B 0 1, Hex A 0 2], [Hex C 1 0, Hex Empty 1 1, Hex E 1 2, Hex B 1 3], [Hex F 2 0, Hex G 2 1, Hex C 2 2], [Hex A 3 0, Hex B 3 1, Hex C 3 2, Hex D 3 3]] 4)


