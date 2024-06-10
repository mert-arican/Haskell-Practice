-- Takes an integer and returns the list of the digits that the number is consisted of.
-- This is a long but efficient implementation O(N).    
toDigits :: Integer -> [Integer]
toDigits number = toDigits' denominator 
    where
        countDigits :: Integer -> Integer
        countDigits number
            | (div number 10) == 0 = 1
            | otherwise = 1 + (countDigits (div number 10))
        
        denominator :: Integer
        denominator = (10 ^ ((countDigits number)-1))

        toDigits' :: Integer -> [Integer]
        toDigits' denominator
            | number <= 0       = []
            | denominator <= 0  = []
            | otherwise         = (mod (div number denominator) 10) : (toDigits' (div denominator 10))



-- Takes an integer and returns the list of the numbers that the number is consisted of
-- in the reverse order.
toDigitsRev :: Integer -> [Integer]
toDigitsRev number
    | number <= 0 = []
    | otherwise = (mod number 10) : (toDigitsRev (div number 10))



-- Doubles every other element of the list starting from the right side of the list.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther numbers = doubleEveryOther' numbers ((mod (length numbers) 2) == 0)
    where
        doubleEveryOther' :: [Integer] -> Bool -> [Integer]
        doubleEveryOther' [] _ = []
        doubleEveryOther' (x:[]) _ = [x]
        doubleEveryOther' (x:y:z) hasEvenNumberOfElements
            | hasEvenNumberOfElements = (2 * x) : y : (doubleEveryOther' z hasEvenNumberOfElements)
            | otherwise               = x : (2 * y) : (doubleEveryOther' z hasEvenNumberOfElements)



-- Sums the all the digits of the numbers in a given list.
sumDigits :: [Integer] -> Integer
sumDigits []    = 0
sumDigits (x:y) = (sumDigitsOfANumber x) + (sumDigits y)
    where
        sumDigitsOfANumber :: Integer -> Integer
        sumDigitsOfANumber number 
            | number == 0   = 0
            | otherwise     = (mod number 10) + (sumDigitsOfANumber (div number 10))



-- Checks whether the given number is a valid credit card number or not.
validate :: Integer -> Bool
validate number = mod (sumDigits (doubleEveryOther (toDigits number))) 10 == 0



cardNumber :: Integer
cardNumber = 4012888888881882



main :: IO()
main = putStrLn(
    -- show (toDigits cardNumber)
    -- show (doubleEveryOther (toDigits cardNumber))
    -- show (sumDigits (doubleEveryOther (toDigits cardNumber)))
    show (validate cardNumber)
    )