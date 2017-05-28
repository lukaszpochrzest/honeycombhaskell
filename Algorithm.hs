module Algorithm (rWT) where
import Debug.Trace (trace)
import Model
import InputConversion
import Utils
import StateGeneration
import EmptyHexFinding

rWT state =     if emptyHex == Nothing
                then Just state
                else tryUntil rWT (generateStatesTrace (unJust emptyHex) state) (\maybeResult -> isJust maybeResult)
                        where emptyHex = findEmptyHex state

tryUntil _ [] _                        = trace ("No valid states") (Nothing)
tryUntil f (s:states) successPredicate = if successPredicate result
                                                then result
                                                else tryUntil f states successPredicate
                                                        where result = f s

