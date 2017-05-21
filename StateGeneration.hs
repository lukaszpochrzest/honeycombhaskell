module StateGeneration where
import Debug.Trace (trace)
import Model
import ModelConversion
import Utils
import StateValidation

-- |Generate possible valoues for Hex considering only its direct neighbours values.
-- TODO extend how deep in neighbour graph do we look up ?
generateValues hex state = foldl intersection allMeaningfulHexValues (map (complement.value) (neighbours hex state))
-- TESTS:
-- generateValues (Hex D 1 1) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex D 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == [D]
-- TODO more tests

generateStatesTrace emptyHex state = trace ("Generating states from state:\n" ++ (showState state)) (generateStates emptyHex state)
generateStates emptyHex state = filter (\newState -> validateNewHex emptyHex newState) (proposedStates emptyHex state)

proposedStates hex state = map (\newValue -> generateState newValue hex state) generatedValues
                                where generatedValues = generateValues hex state

generateState newValue hex state = insertHex (Hex newValue (getRow hex) (getColumn hex)) state
-- TESTS: TODO better tests
-- generateStates (Hex Empty 1 1) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex Empty 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == [State [[Hex A 0 0,Hex B 0 1],[Hex C 1 0,Hex D 1 1,Hex E 1 2],[Hex F 2 0,Hex G 2 1]] 3]
