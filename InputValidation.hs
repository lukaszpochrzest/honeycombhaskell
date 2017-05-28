module InputValidation (validate) where
import InputConversion

validate plaster = if lengthsOf (evenlines plaster) (n-1) && lengthsOf (oddlines plaster) n then plaster else plaster
                                where n = length (getLines plaster)
-- TEST:
--validate (Plaster ["abc", "abcd", "abc", "abcd"])==True
--validate (Plaster ["abc", "abcd", "abc"]) -- throws Exception


evenlines (Plaster lines) = evenlinesInternal lines 0

evenlinesInternal [] _ = []
evenlinesInternal (l:ls) index | even index == True = l:(evenlinesInternal ls (index+1))
                               | otherwise          = evenlinesInternal ls (index+1)


oddlines (Plaster lines) = oddlinesInternal lines 0

oddlinesInternal [] _ = []
oddlinesInternal (l:ls) index | odd index == True = l:(oddlinesInternal ls (index+1))
                              | otherwise         = oddlinesInternal ls (index+1)



lengthsOf [] _ = True
lengthsOf (l:lists) k = if length l == k then lengthsOf lists k else error ("ERROR! Invalid input: line " ++ l ++ " should have " ++ show(k) ++ " symbols")
