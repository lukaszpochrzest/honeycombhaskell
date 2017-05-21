module Main where
import System.Environment
import Convert
import Algorithm

main = do       args <- getArgs
                content <- readFile (args !! 0)
                print (rWT (parseLines (lines content)))
