module ValueGeneration (generateValues) where
import Model
import NeighboursFinding
import Utils

-- | Returns possible values generated for Hex considering only its neighbours and its neighbours' neighbours
generateValues hex state = intersectComplementValuesOf (neighboursNeighbours hex state)
-- TESTS:
-- generateValues (Hex D 1 1) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex D 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == [D]
-- TODO more tests

-- | Returns intersection of lists of values that are complement to hexes values
intersectComplementValuesOf hexes = foldl intersection allMeaningfulHexValues (map (complement.value) hexes)

-- | Returns list of all meaningful hex values
allMeaningfulHexValues = [A,B,C,D,E,F,G]

-- TODO comment TODO it doesnt have to be hardcoded (iterate through and remove if equals)
complement A = [B,C,D,E,F,G]
complement B = [A,C,D,E,F,G]
complement C = [A,B,D,E,F,G]
complement D = [A,B,C,E,F,G]
complement E = [A,B,C,D,F,G]
complement F = [A,B,C,D,E,G]
complement G = [A,B,C,D,E,F]
complement Empty = [A,B,C,D,E,F,G]
