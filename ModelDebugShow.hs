module ModelDebugShow (showState) where
import Model

-- | converts State to String
showState state = showLines (getHoneycomb state) 0

showLines [] n = []
showLines (hexLine:hexLines) n | even n == True = " " ++ (showHexes hexLine) ++ "\n" ++ (showLines hexLines (n+1))
                               | otherwise      = (showHexes hexLine) ++ "\n" ++ (showLines hexLines (n+1))

showHexes [] = []
showHexes (hex:hexes) = (showValue (value hex)):' ':(showHexes hexes)

showValue A = 'A'
showValue B = 'B'
showValue C = 'C'
showValue D = 'D'
showValue E = 'E'
showValue F = 'F'
showValue G = 'G'
showValue Empty = '-'
