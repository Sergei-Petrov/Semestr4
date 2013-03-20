divisors :: Integral a => a -> [a]
divisors n = [x | x <- [1..(n - 1)], rem n x == 0]

primes = [n | n <- [1..], isPrime n]
	where isPrime x = (divisors x == [1])

func :: Int -> [Int]
func a = take a primeList