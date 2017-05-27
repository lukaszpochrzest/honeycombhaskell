module Main where
import System.Environment
import Model
import InputConversion
import InputValidation
import Algorithm
import Utils

main = do       args <- getArgs
                content <- readFile (args !! 0)
                print (showState (unJust (rWT (convertPlasterToState (validate((read content)::Plaster)) ))))
