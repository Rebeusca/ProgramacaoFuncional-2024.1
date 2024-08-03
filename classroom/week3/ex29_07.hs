-- Aula 29/07/24 - Segunda - feira;
-- Funções curry.

teste :: t1 -> t2 -> (t1 -> t2 -> t3) -> t3
teste x y f = f x y

-- Função para somar os elementos de uma lista
--sum' :: Num a1 => p -> [a2] -> a1
--sum' ls [] = 0
--sum' [a] = a
--sum' (x:xs) = x + sum' xs

-- map (^2) [1..10]
-- map (2^) [1..10]	

-- Função que receba uma lista de tuplas e retorne a soma dos elementos entre elas
st :: Num a => (a, a) -> a
st (x, y) = x + y
-- Função que receba uma lista de tuplas e retorne a soma dos elementos entre elas
sumTuplas :: Num a => [(a, a)] -> [a]
sumTuplas [] = []
sumTuplas ls = map st ls

-- superSumTuplas :: Num a => [(a, a)] -> a
-- superSumTuplas ls = sum (sumTuplas ls)
-- superSumTuplas ls = sum $ sumTuplas ls
superSumTuplas ls = map (fst + snd) ls
