module ValueGeneration (generateValues) where
import Model
import NeighboursFinding
import Utils

-- | Evaluates to possible values generated for given Hex considering only its neighbours and its neighbours' neighbours
generateValues hex state = intersectComplementValuesOf (neighboursNeighbours hex state)
-- TESTS:
-- generateValues (Hex D 1 1) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex D 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == [D]
-- TODO more tests

-- | Evaluates to intersection of lists of values that are complement to given hexes values
intersectComplementValuesOf hexes = foldl intersection allMeaningfulHexValues (map (complement.value) hexes)

-- | Evaluates to list of all meaningful HexValues
allMeaningfulHexValues = [A,B,C,D,E,F,G]

-- | Evaluates to complement values to given HexValue
complement A = [B,C,D,E,F,G]
complement B = [A,C,D,E,F,G]
complement C = [A,B,D,E,F,G]
complement D = [A,B,C,E,F,G]
complement E = [A,B,C,D,F,G]
complement F = [A,B,C,D,E,G]
complement G = [A,B,C,D,E,F]
complement Empty = [A,B,C,D,E,F,G]
