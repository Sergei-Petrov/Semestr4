findPos :: (a -> Bool) -> [a] -> [Int]
findPos p xs = [ i | (x,i) <- zip xs [0..], p x]

func :: Eq a => a -> [a] -> [Int]
func x = findPos (x==)