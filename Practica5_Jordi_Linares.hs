-- EJERCICIO 1
resto :: Int -> Int -> Int
resto x y  
    | x - y < 0 = x
    | otherwise = resto (x-y) y

-- EJERCICIO 2
sumatorio :: Int -> Int -> Int
sumatorio x y
    | x > y = 0
    | otherwise = x + sumatorio (x+1) y

-- EJERCICIO 3
reverso :: [x] -> [x]
reverso x
    | null(x) = []
    | otherwise = [last x] ++ reverso (init x)

-- EJERCICIO 4
divisible :: Int -> Int -> Bool
divisible x y
    | x `mod` y == 0 = True
    | otherwise = False 

-- EJERCICIO 5
divisores :: Int -> [Int]
divisores n = [x | x <- [2..(n-1)], divisible n x == True]

-- EJERCICIO 6
divisoresRaiz :: Int -> [Int]
divisoresRaiz n = [x | x <- [2.. floor (sqrt (fromIntegral n))], divisible n x == True]

primos :: Int -> [Int]
primos n = [x | x <- [2..(n-1)], length (divisoresRaiz x) == 0]

-- EJERCICIO 7
decBin :: Int -> [Int]
decBin x 
    | x == 0 || x == 1 = [x]
    | x > 1 = decBin (floor (fromIntegral x/2)) ++ [x `mod` 2]
    | otherwise = error "Negative number" --Para que funcione la excepción, el valor negativo de x ha de ir entre paréntesis, por ejemplo (-1)

-- EJERCICIO 8
binDec :: [Int] -> Int
binDec x 
    | null(x) == True || x == [0] = 0
    | x == [1] = 1
    | last x == 0 = 2 * binDec (init x)
    | last x == 1 = 1 + 2 * binDec (init x)
    | otherwise = error "Incorrect values, only 0 or 1 accepted"

-- EJERCICIO 9
sinRep :: [Int] -> [Int]
sinRep n = [x | x <- [minimum n..maximum n], elem x n == True]


length' :: [a] -> Int
length' [] = 0
length' l = 1 + length' (init(l))
