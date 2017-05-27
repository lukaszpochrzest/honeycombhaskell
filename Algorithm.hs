module Algorithm where
import Debug.Trace (trace)
import Model
import InputConversion
import Utils
import StateGeneration

-- | Finds empty hex in state
findEmptyHex state = findEmptyHexInHoneycomb (getHoneycomb state)
findEmptyHexInHoneycomb [] = Nothing
findEmptyHexInHoneycomb (row:rows) = if hexFound /= Nothing then hexFound else findEmptyHexInHoneycomb rows
                                        where hexFound = findEmptyHexInRow row
findEmptyHexInRow [] = Nothing
findEmptyHexInRow (h:hs) = if (value h) == Empty then Just h else findEmptyHexInRow hs
-- TESTS
-- findEmptyHex (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex Empty 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == Just (Hex Empty 1 1)

rWT state =     if emptyHex == Nothing
                then Just state
                else tryUntil rWT (generateStatesTrace (unJust emptyHex) state) (\maybeResult -> isJust maybeResult)
                        where emptyHex = findEmptyHex state

tryUntil _ [] _                        = trace ("No valid states") (Nothing)
tryUntil f (s:states) successPredicate = if successPredicate result
                                                then result
                                                else tryUntil f states successPredicate
                                                        where result = f s

