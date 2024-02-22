-- Trabajo realizado por:
-- Lizzeth Magaña Domínguez
-- Pedro Antonio Ayala Ramos

-- Ejercicio 1.  Suma de elementos en una lista
sumarLista :: [Int] -> Int
sumarLista [] = 0
sumarLista (x:xs) = x + sumarLista xs

-- Ejercicio 2. Factorial
factorial :: Int -> Int
factorial n
    | n == 0    = 1
    | otherwise = n * factorial (n - 1)

-- Ejercicio 3. Números pares
numerosPares :: Int -> [Int]
numerosPares n = [x | x <- [1..n], even x]

-- Ejercicio 4. Longitud de una cadena
longitudCadena :: String -> Int
longitudCadena cadena = length cadena

-- Ejercicio 5. Reverso de una lista
reversoLista :: [a] -> [a]
reversoLista val = reverse val

-- Ejercicio 6. Duplicar elementos
duplicarElementos :: [Int] -> [Int]
duplicarElementos [] = []
duplicarElementos (val1:restval) = val1 * 2 : duplicarElementos restval
-- Ejercicio 7. Filtrar elementos pares
filtrarPares :: [Int] -> [Int]
filtrarPares [] = []
filtrarPares (x:y)
    | even x = x : filtrarPares y
    | otherwise = filtrarPares y

-- Ejercicio 8. Fibonacci
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)

-- Ejercicio 9. Divisores de un número
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

-- Ejercicio 10. Palíndromo
esPalindromo :: String -> Bool
esPalindromo s = s == reverse s