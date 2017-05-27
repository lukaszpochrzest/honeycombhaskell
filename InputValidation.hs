module InputValidation where
import InputConversion

validate plaster = lengthsOf (evenlines plaster) (n-1) && lengthsOf (oddlines plaster) n
                                where n = length (getLines plaster)
-- TEST:
--validate (Plaster ["abc", "abcd", "abc", "abcd"])==True
--validate (Plaster ["abc", "abcd", "abc"]) -- throews Exception


evenlines (Plaster lines) = evenlinesInternal lines 0

evenlinesInternal [] _ = []
evenlinesInternal (l:ls) index | even index == True = l:(evenlinesInternal ls (index+1))
                               | otherwise          = evenlinesInternal ls (index+1)


oddlines (Plaster lines) = oddlinesInternal lines 0

oddlinesInternal [] _ = []
oddlinesInternal (l:ls) index | odd index == True = l:(oddlinesInternal ls (index+1))
                              | otherwise         = oddlinesInternal ls (index+1)



lengthsOf [] _ = True
lengthsOf (l:lists) k = if length l == k then lengthsOf lists k else error "ERROR! Invalid input"
