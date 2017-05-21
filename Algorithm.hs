module Algorithm where
import Debug.Trace (trace)
import Model
import Utils

--
-- HEX VALUE GENERATION
--

-- Generate possible valoues for Hex considering only its direct neighbours values.
-- TODO extend how deep in neighbour graph do we look up ?
generateValues hex state = foldl intersection allMeaningfulHexValues (map (complement.value) (neighbours hex state))
-- TESTS:
-- generateValues (Hex D 1 1) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex D 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == [D]
-- TODO more tests

--
-- HEX VALUE VALIDATION
--

-- TODO comment
--validateHexWithItsDirectNeighbours hex = not (elem (value hex) (map value (neighbours hex))) // Thats optimization that cannot be used in validateHex in parents part

validateHexWithItsDirectNeighbours hex state = (not.duplicates) (filter (\v -> v/= Empty)((value hex):(map value (neighbours hex state))))
-- TESTS:
-- validateHexWithItsDirectNeighbours (Hex D 1 1) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex D 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == True
-- validateHexWithItsDirectNeighbours (Hex A 1 1) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex D 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == False

-- TODO comment
validateHex hex state = trace ("trace validateHex:" ++ show hex) (all (\neigh -> validateHexWithItsDirectNeighbours neigh state) (neighbours hex state) && -- TODO optimize
                            validateHexWithItsDirectNeighbours hex state) -- remove that - hex value generation assures its always true
-- TESTS:
-- validateHex (Hex D 1 1) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex D 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == True
-- validateHex (Hex A 1 1) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex D 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == False
-- TODO better tests (neigh of neighs)

--
-- STATE GENERATION
--
xxx hex state = (map (\newValue -> generateState newValue hex state) [B])
generateStates hex state = filter (\newState -> validateHex (getHex newState (getRow hex) (getColumn hex) ) newState) (map (\newValue -> generateState newValue hex state) generatedValues) 
                                where generatedValues = generateValues hex state

generateState newValue hex state = insertHex (Hex newValue (getRow hex) (getColumn hex)) state
-- TESTS: TODO better tests
-- generateStates (Hex Empty 1 1) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex Empty 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == [State [[Hex A 0 0,Hex B 0 1],[Hex C 1 0,Hex D 1 1,Hex E 1 2],[Hex F 2 0,Hex G 2 1]] 3]



--
-- ALGORITHM
--

findEmptyHex state = findEmptyHexInHoneycomb (getHoneycomb state)
findEmptyHexInHoneycomb [] = Nothing
findEmptyHexInHoneycomb (row:rows) = if hexFound /= Nothing then hexFound else findEmptyHexInHoneycomb rows
                                        where hexFound = findEmptyHexInRow row
findEmptyHexInRow [] = Nothing
findEmptyHexInRow (h:hs) = if (value h) == Empty then Just h else findEmptyHexInRow hs
-- TESTS
-- findEmptyHex (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex Empty 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == Just (Hex Empty 1 1)

rWT state = if emptyHex == Nothing then Just state else tryUntil rWT (generateStates (unJust emptyHex) state) (\maybeResult -> isJust maybeResult)
                where emptyHex = findEmptyHex state

tryUntil _ [] _                        = Nothing
tryUntil f (s:states) successPredicate = if successPredicate result
                                                then result
                                                else tryUntil f states successPredicate
                                                        where result = f s

