import Text.XHtml (col)
-- Aula 15/07/2024 - Segunda-feira;
-- Guardas.

-- Imagine que você tem um número inteiro e você deve formar uma série a partir desse número inteiro,
-- onde esse número é o primeiro da série e o seguinte é dado por:
-- - Se o número é par, o próximo é o número dividido por 2
-- - Se o número é ímpar, o próximo é 3 vezes o número mais 1
-- A série termina quando o número é 1.
-- Implemente a função collatz :: Int -> [Int] que recebe um número inteiro e retorna a série de Collatz desse número.
-- Exemplo de execução:
-- collatz 6 == [6,3,10,5,16,8,4,2,1]
-- collatz 7 == [7,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1]
-- collatz 1 == [1]
-- collatz :: Int -> [Int]
-- collatz 1 = [1]
-- collatz n
--     | mod n 2 == 0 = n : collatz (div n 2) -- Se par, divide por 2
--     | otherwise = n : collatz (3 * n + 1) -- Se ímpar, multiplica por 3 e soma 1

collatz_int :: Int -> Int
collatz_int n
    | mod n 2 == 0 = div n 2
    | otherwise = 3 * n + 1

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n = n : collatz (collatz_int n)

-- Entre os número de 1 a 1000 qual aquele que tem a serie de collatz de maior comprimento?
collatz_length :: Int
collatz_length = maximum [length (collatz n) | n <- [1..1000]]

collatz_maxNumber :: Int
collatz_maxNumber = head [n | n <- [1..1000], length (collatz n) == collatz_length]


