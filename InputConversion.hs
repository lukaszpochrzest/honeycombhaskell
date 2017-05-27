module InputConversion where
import Data.Char
import Model

-- | Converts String to State
parseLines lines = State (parseInternal lines 0) (length lines)

parseInternal [] _          = []
parseInternal (l:lines) row = (parseLine l 0 row):(parseInternal lines (row+1))

parseLine [] _ _               = []
parseLine (c:chars) column row = (Hex (valueOfChar c) row column):(parseLine chars (column+1) row)

valueOfChar '.' = Empty
valueOfChar 'A' = A
valueOfChar 'B' = B
valueOfChar 'C' = C
valueOfChar 'D' = D
valueOfChar 'E' = E
valueOfChar 'F' = F
valueOfChar 'G' = G
valueOfChar x   = error ("Error raised! Cannot parse: " ++ (x:[])) -- TODO char

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



--
{-
stringToPlasterParser = do      char 'P'
                                char 'l'
                                char 'a'
                                char 's'
                                char 't'
                                char 'e'
                                char 'r'
                                char ' '
                                char '['
                                char '\"'
                                char '\"'
                                char ']'
                                return (State [[]] 0)
-}

--parse stringToPlasterParser "Plaster [\"\"]"


data Plaster = Plaster [[Char]] deriving (Show, Read)

getLines (Plaster lines) = lines

convertPlasterToState (Plaster lines) = parseLines lines
