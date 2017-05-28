module StateGeneration (generateStatesTrace) where
import Debug.Trace (trace)
import ModelDebugShow
import Model
import StateValidation
import ValueGeneration
import Utils

generateStatesTrace emptyHex state = trace ("Generating states from state:\n" ++ (showState state)) (generateStates emptyHex state)

-- TODO no validation here
-- | Generates new states
generateStates emptyHex state = filter (\newState -> validateNewHex emptyHex newState) (proposedStates emptyHex state)

-- | Generates new states
proposedStates hex state = generateStatesFromValues generatedValues hex state
                                where generatedValues = generateValues hex state

-- | Generates new states using values generation
generateStatesFromValues generatedValues hex state = trace ("Generated values:" ++ (show generatedValues)) (map (\newValue -> generateState newValue hex state) generatedValues)

-- | Generates state using new HexValue
generateState newValue hex state = insertHex (Hex newValue (getRow hex) (getColumn hex)) state

-- TESTS: TODO better tests
-- generateStates (Hex Empty 1 1) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex Empty 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == [State [[Hex A 0 0,Hex B 0 1],[Hex C 1 0,Hex D 1 1,Hex E 1 2],[Hex F 2 0,Hex G 2 1]] 3]

-- |Creates new state by inserting hex into an old state
insertHex hex (State honeycomb n) = State (swapInMatrixAt hex honeycomb (getColumn hex) (getRow hex) ) n

--TESTS
-- insertHex (Hex A 2 1) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex D 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == State [[Hex A 0 0,Hex B 0 1],[Hex C 1 0,Hex D 1 1,Hex E 1 2],[Hex F 2 0,Hex A 2 1]] 3
