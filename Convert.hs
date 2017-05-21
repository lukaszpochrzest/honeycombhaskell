module Convert where
import Model

parseLines lines = State (parseInternal lines 0) (length lines)

parseInternal [] _          = []
parseInternal (l:lines) row = (parseLine l 0 row):(parseInternal lines (row+1))

parseLine [] _ _               = []
parseLine (c:chars) column row = (Hex (valueOfChar c) row column):(parseLine chars (column+1) row)

valueOfChar '-' = Empty
valueOfChar 'A' = A
valueOfChar 'B' = B
valueOfChar 'C' = C
valueOfChar 'D' = D
valueOfChar 'E' = E
valueOfChar 'F' = F
valueOfChar 'G' = G
valueOfChar x   = error "Cannot parse" -- TODO char
