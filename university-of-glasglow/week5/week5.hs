properFactors :: Int -> [Int]
properFactors x = filter (\y -> (x `mod` y == 0)) [2..(x - 1)]

numProperFactors :: Int -> Int
numProperFactors x = length (properFactors x)

-- infinite list of primes
primes :: [Int]
primes = filter (\x -> numProperFactors x == 0) [2..]

f' :: Int -> Int
f' x = x + 1

-- lambda expression
f'' :: Int -> Int -> Int -> Int
f'' = \x y z -> 2*x + y*z

