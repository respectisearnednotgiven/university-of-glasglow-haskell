-- using let-in
journeyCost :: Float -> Float -> Float
journeyCost miles fuelCostPerLitre =
    let milesPerGallon = 35
        litresPerGallon = 4.55
        gallons = miles / milesPerGallon
    in (gallons * litresPerGallon * fuelCostPerLitre)

-- using where
squarePlusOne :: Int -> Int
squarePlusOne x = xSquared + 1
    where xSquared = x*x

-- using multiple where statements
celsToFahr :: Float -> Float
celsToFahr deg = (deg*scalingFactor) + freezingPoint
    where scalingFactor = 9.0/5.0
          freezingPoint = 32

-- using if-then-else statements
absolute :: (Ord p, Num p) => p -> p
absolute x = if x < 0 then (-x) else x

-- using guards
absolute' :: (Ord p, Num p) => p -> p
absolute' x
    | x < 0 = -x
    | otherwise = x

-- using guards and a where statement
holeScore :: Int -> Int -> String
holeScore strokes par
    | score < 0 = show (abs score) ++ " under par"
    | score == 0 = "level par"
    | otherwise = show score ++ " over par"
    where score = strokes - par

-- algebraic data types
data Pet = Cat | Dog | Fish | Parrot String

-- case expression
hello :: Pet -> [Char]
hello animal =
    case animal of
        Cat -> "meeow"
        Dog -> "woof"
        Fish -> "bubble"
        Parrot name -> "pretty " ++ name

-- find the max value in the Int list

maxHelper :: Int -> [Int] -> Int
maxHelper = foldl (\ x y -> if x > y then x else y)

maxFromList :: [Int] -> Maybe Int
maxFromList [] = Nothing
maxFromList (x:xs) = Just (maxHelper x xs) -- just is used to wrap this inside the Maybe

