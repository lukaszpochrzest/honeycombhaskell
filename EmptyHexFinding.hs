module EmptyHexFinding (findEmptyHex) where
import Model
import Utils

-- | Finds empty hex in state
findEmptyHex state = findInMatrix (getHoneycomb state) (\hex -> (value hex) == Empty)

-- TESTS
-- findEmptyHex (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex Empty 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == Just (Hex Empty 1 1)
