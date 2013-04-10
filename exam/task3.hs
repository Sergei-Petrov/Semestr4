diamondHelp n (-1) = []
diamondHelp n k = (line (n - abs (n - k))) ++ (diamondHelp n (k - 1))
	where
		func 0 0 = "\n"
		func k 0 = '*' : func (k - 1) 0
		func k m = ' ' : func k (m - 1)
		line k = func (1 + k * 2) (n - k)

diamond :: Int -> String
diamond n = diamondHelp (n - 1) (2 *(n - 1))