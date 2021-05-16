journeyCost :: Float -> Float -> Float
journeyCost miles fuelCostPerLitre =
    let milesPerGallon = 35
        litresPerGallon = 4.55
        gallons = miles / milesPerGallon
    in (gallons * litresPerGallon * fuelCostPerLitre)

squarePlusOne :: Int -> Int
squarePlusOne x = xSquared + 1
    where xSquared = x*x

celsToFahr :: Float -> Float
celsToFahr deg = (deg*scalingFactor) + freezingPoint
    where scalingFactor = 9.0/5.0
          freezingPoint = 32

