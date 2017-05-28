module Main where
import System.Environment
import Model
import InputConversion
import InputValidation
import InputModel
import Algorithm
import Utils

main = do       args <- getArgs
                content <- readFile (args !! 0)
                print (showState (unJust (rWT (plasterToState (validate((read content)::Plaster)) ))))
