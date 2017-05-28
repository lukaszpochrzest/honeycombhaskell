module StateValidation (validateNewHex) where
import Debug.Trace (trace)
import Model
import NeighboursFinding
import InputConversion
import Utils

validateNewHex emptyHex newState = if validateHex (getNewHex emptyHex newState) newState == True then True else error "ERROR! Validation failed"
getNewHex oldHex newState = getHex newState (getRow oldHex) (getColumn oldHex)

-- TODO comment
--validateNeighbourhood hex = not (elem (value hex) (map value (neighbours hex))) // Thats optimization that cannot be used in validateHex in parents part

validateNeighbourhood hex state = (not.duplicates) (filter (\v -> v/= Empty) (neighbourhoodValues hex state))
neighbourhoodValues hex state = (value hex):(neighboursValues hex state)
neighboursValues hex state = map value (neighbours hex state)

-- TESTS:
-- validateNeighbourhood (Hex D 1 1) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex D 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == True
-- validateNeighbourhood (Hex A 1 1) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex D 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == False

-- | Validates new hex in context of an old valid state. This function makes sense only if state is already valid
validateHex hex state = validateNeighboursNeighbourhoods hex state && validateNeighbourhood hex state -- second argument can be removed - hex value generation assures its always true
validateNeighboursNeighbourhoods hex state = all (\neighbour -> validateNeighbourhood neighbour state) (neighbours hex state)

-- TESTS:
-- validateHex (Hex D 1 1) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex D 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == True
-- validateHex (Hex A 1 1) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex D 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == False
-- TODO better tests (neigh of neighs)
