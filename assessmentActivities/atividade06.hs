atividade = "06"
nome = "Rebeca Amorim Penha"
matricula = ""

-- 1
-- crie uma função que determine se uma string é anagrama de outra
-- Função para inserir um elemento na posição correta em uma lista ordenada
inserir :: Char -> [Char] -> [Char]
inserir x [] = [x]
inserir x (y:ys)
    | x <= y    = x : y : ys
    | otherwise = y : inserir x ys

-- Função de ordenação por inserção
ordenar :: [Char] -> [Char]
ordenar [] = []
ordenar (x:xs) = inserir x (ordenar xs)

-- Função para determinar se uma string é anagrama de outra
anagrama :: [Char] -> [Char] -> Bool
anagrama str1 str2 = ordenar str1 == ordenar str2

-- 2
-- construa função que elimine repetções de uma dada string s
--  sem alterar a sequência original 
-- dos caracteres de s.
-- Função auxiliar para verificar se um caractere já apareceu na lista
elem' :: Char -> [Char] -> Bool
elem' _ [] = False
elem' x (y:ys) = (x == y) || elem' x ys

-- Função principal para eliminar repetições sem alterar a sequência original
unique :: [Char] -> [Char]
unique [] = []
unique (x:xs)
    | elem' x xs = unique xs
    | otherwise  = x : unique xs

-- 3
-- implemente uma função que determine a string formada pelos 
-- caracteres comuns a duas strins de entrada a e b. A saida não 
-- deve ter duplicadas.
-- Função auxiliar para verificar se um caractere está presente na lista
elem'2 :: Char -> [Char] -> Bool
elem'2 _ [] = False
elem'2 x (y:ys) = (x == y) || elem' x ys

-- Função principal para encontrar a intersecção de duas strings
-- sem duplicar os caracteres na saída
intersec :: [Char] -> [Char] -> [Char]
intersec [] _ = []
intersec (x:xs) ys
    | elem'2 x ys && not (elem'2 x xs) = x : intersec xs ys
    | otherwise = intersec xs ys

-- 4
-- dado três listas zipálas numa lista de triplas de forma 
-- semelhante ao comando zip. 
zip'linha :: [a] -> [b] -> [c] -> [(a,b,c)]
zip'linha (x:xs) (y:ys) (z:zs) = (x,y,z) : zip'linha xs ys zs
zip'linha _ _ _ = []
