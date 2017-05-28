module InputConversion (plasterToState) where
import Model
import InputModel
import Utils

-- | Maps Plaster To State
plasterToState (Plaster plaster) = State (mapMatrix charToHex plaster 0) (length plaster)

-- | Maps char and coordinates to Hex
charToHex = \c column row -> Hex (charToHexValue c) row column

-- | Maps char to HexValue
charToHexValue '.' = Empty
charToHexValue 'A' = A
charToHexValue 'B' = B
charToHexValue 'C' = C
charToHexValue 'D' = D
charToHexValue 'E' = E
charToHexValue 'F' = F
charToHexValue 'G' = G
charToHexValue x   = error ("Error raised! Cannot parse: " ++ (x:[])) -- TODO char

