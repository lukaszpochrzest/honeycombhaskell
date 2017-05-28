module Algorithm (backtracking) where
import Debug.Trace (trace)
import Model
import Utils
import StateGeneration
import EmptyHexFinding

-- | Backtracking
backtracking state =    if emptyHex == Nothing
                        then Just state
                        else tryUntil backtracking (generateStatesTrace (unJust emptyHex) state) (\maybeResult -> isJust maybeResult)
                                where emptyHex = findEmptyHex state

-- | Applies function to states form list until return value doesnt satisfy predicate, then returns this value
tryUntil _ [] _                        = trace ("No valid states") (Nothing)
tryUntil f (s:states) successPredicate = if successPredicate result
                                                then result
                                                else tryUntil f states successPredicate
                                                        where result = f s

