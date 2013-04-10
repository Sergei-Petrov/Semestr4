import Data.Maybe

CheckBrackets :: String -> Bool
CheckBrackets = func "" . filter (`elem` "()<>{}[]")
     where func stack (c:cs) = if opening c
                                         then func (c:stack) cs
                                         else if (null stack || not (pair (head stack) c))
                                                  then False
                                                  else func (tail stack) cs
           opening c = c `elem` "(<{["
           pair c1 c2 = (c1:c2:[]) `elem` ["()", "<>", "{}", "[]"]




