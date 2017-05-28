module Main where
import System.Environment
import ModelDebugShow
import Model
import InputConversion
import InputValidation
import InputModel
import Algorithm
import Utils

main = do       args <- getArgs
                content <- readFile (args !! 0)
                putStrLn (showState (unJust (backtracking (plasterToState (validate((read content)::Plaster)) ))))
