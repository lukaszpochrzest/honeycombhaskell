module Model where
import Utils

--imports 
import Debug.Trace (trace)

--
-- MODEL
--

-- Hex HexValue column row
data Hex = Hex HexValue Int Int deriving (Eq, Show)
data HexValue = Empty | A | B | C | D | E | F | G deriving (Eq, Show)

-- [[Hex]] n
data State = State [[Hex]] Int deriving (Show)

-- MODEL HELPER METHODS

-- TODO comment
value (Hex val _ _) = val
getRow (Hex _ row _) = row
getColumn (Hex _ _ column) = column
getHoneycomb (State honeycomb _) = honeycomb

getHex (State honeycomb _) row column = (honeycomb !! row) !! column
--removeHex (Hex _ row column) (State honeycomb _) =  romoveHexFromRow oldHexRow
--                                                        where oldHexRow
--insertRow newRow (State honeycomb _) n = insertAt newRow honeycomb n
--removeRow (State honeycomb _) n = removeAt honeycomb n
--insertHexInRow honeycombRow hex = insertAt hex honeycombRow (getColumn hex)
--removeHexFromRow honeycombRow hex =  removeAt honeycombRow (getColumn hex)

insertHex hex (State honeycomb n) = State (insertHexInHoneycomb hex honeycomb 0) n
insertHexInHoneycomb hex (row:rows) currRowIndex | (getRow hex) == currRowIndex = (insertHexInRow hex row 0):rows
                                                 | otherwise                    = row:(insertHexInHoneycomb hex rows (currRowIndex + 1)) 
insertHexInRow hex (h:hs) currentHIndex | (getColumn hex) == currentHIndex = hex:hs
                                        | otherwise                        = h:(insertHexInRow hex hs (currentHIndex+1))
--TESTS
-- insertHex (Hex A 2 1) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex D 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == State [[Hex A 0 0,Hex B 0 1],[Hex C 1 0,Hex D 1 1,Hex E 1 2],[Hex F 2 0,Hex A 2 1]] 3

getN (State _ n) = n

isLastRow (Hex _ row _) (State _ n) = row + 1 == n

isTopRow (Hex _ row _) _ = row == 0

isLastColumn (Hex _ row column) (State _ n) | even row  = column + 1 == n - 1
                                            | otherwise = column + 1 == n

isFirstColumn (Hex _ _ column) _ = column == 0

-- TODO comment
neighbours hex state = map unJust (filter isJust ((getNENeighbour hex state):(getENeighbour hex state):(getSENeighbour hex state):(getSWNeighbour hex state):(getWNeighbour hex state):(getNWNeighbour hex state):[]))--getENeighbour:getSENeighbour:getSWNeighbour:getWNeighbour:getNWNeighbour

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

-- TODO comment
allMeaningfulHexValues = [A,B,C,D,E,F,G]

--
-- HELPER FUNCTIONS
--

-- TODO comment TODO it doesnt have to be hardcoded (iterate through and remove if equals)
complement A = [B,C,D,E,F,G]
complement B = [A,C,D,E,F,G]
complement C = [A,B,D,E,F,G]
complement D = [A,B,C,E,F,G]
complement E = [A,B,C,D,F,G]
complement F = [A,B,C,D,E,G]
complement G = [A,B,C,D,E,F]
complement Empty = [A,B,C,D,E,F,G]
-- TESTS:


