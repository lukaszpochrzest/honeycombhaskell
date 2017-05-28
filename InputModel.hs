module InputModel where

data Plaster = Plaster [[Char]] deriving (Show, Read)

getLines (Plaster lines) = lines
