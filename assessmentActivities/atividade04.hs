atividade = "04"
nome = "Rebeca Amorim Penha"
matricula = ""

-- 1
replace :: [Char] -> [Char] -> [Char] -> [Char]
replace [] _ _ = []
replace text@(x:xs) from to
                        | take (length from) text == from = to ++ replace (drop (length from) text) from to
                        | otherwise = x : replace xs from to

-- 2
lsSplit :: [Int] -> ([Int], Int, [Int])
lsSplit [] = ([], 0, [])
lsSplit ls = (take pos ls, n, drop (pos + 1) ls)
    where
        n = maximum ls
        pos = maxIndex ls n

maxIndex :: [Int] -> Int -> Int
maxIndex ls n = maxIndexHelper ls n 0

maxIndexHelper :: [Int] -> Int -> Int -> Int
maxIndexHelper [] _ _ = error "Valor não encontrado"
maxIndexHelper (x:xs) n i
                        | x == n = i
                        | otherwise = maxIndexHelper xs n (i + 1) 

-- 3
selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort [x] = [x]
selectionSort ls = selectionSort (removeMax ls) ++ [findMax ls]
    where
        findMax :: Ord a => [a] -> a
        findMax [x] = x
        findMax (x:xs) = if x > findMax xs 
                        then x
                        else findMax xs

        removeMax :: Ord a => [a] -> [a]
        removeMax [] = []
        removeMax [x] = []
        removeMax (x:xs) = if x == findMax (x:xs)
                        then xs
                        else x : removeMax xs