-- Aula 23/07/24 - Terça - feira;
-- Casamento de padrão.

func :: (Eq a, Fractional a) => a -> a -> a
func _ 0 = error "error: Divisão por zero!"
func a b = a / b

-- Faça uma função que receba uma lista de números e retorne a soma dos quadrados dos números pares.
-- sumSquares :: [Int] -> Int
sumSquares :: [Int] -> Int
sumSquares [] = 0
sumSquares (x:xs)
    | mod x 2 == 0 = x^2 + sumSquares xs
    | otherwise = sumSquares xs

-- Implementação da função fst
fst' :: (a, b) -> a
fst' (a, _) = a
-- Implementação da função snd
snd' :: (a, b) -> b
snd' (_, b) = b

-- Case expressions
area :: Floating a => a -> a -> String -> a
area a b tipo = case tipo of
    "retangulo" -> a * b
    "triangulo" -> a * b / 2
    "elipse" -> a * b * pi
    _ -> error "Tipo de figura não suportado!"

-- Let in
-- Faça uma função que receba um número e retorne o quadrado da soma desse número com 1.
-- squarePlusOne :: Int -> Int
squarePlusOne :: Int -> Int
squarePlusOne n = let x = n + 1 in x^2

bhaskara :: Floating b => b -> b -> b -> (b, b)
bhaskara a b c = let delta = b^2 - 4 * a * c
                     x1 = (-b + sqrt delta) / (2 * a)
                     x2 = (-b - sqrt delta) / (2 * a)
                 in (x1, x2)

-- Faça um exemplo de casamento padrão usando @
f :: (a, b) -> (a, b)
f t@(a, _) = (a, snd t)

-- Faça uma função que identifique o n-ésimo elemento de uma lista sem usar o operador !!.
-- Entrada: a lista e a posição j.
-- Saída: o elemento da posição j.
-- nthElement :: [a] -> Int -> a
-- nthElement :: [a] -> Int -> a
-- nthElement [] _ = error "Lista vazia!"
-- nthElement (x:_) 0 = x
-- nthElement (_:xs) j = nthElement xs (j - 1)
nthElement :: [a] -> Int -> a
nthElement (x:xs) 0 = x
nthElement w@(x:xs) j =
  let len = length w
  in if j > 0 && j < len
     then nthElement xs (j - 1)
     else error "Posição inválida!"
nthElement [] _ = error "Lista vazia!"

