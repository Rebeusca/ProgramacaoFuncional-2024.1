atividade = "03"
nome = "Rebeca Amorim Penha"
matricula = ""

-- 1
tls :: String -> [(Char, Int)]
tls s = [(c, count c s) | c <- uniqueChars s]
    where
        uniqueChars :: String -> String
        uniqueChars [] = []
        uniqueChars (x:xs)
            | x `elem'` xs = uniqueChars xs
            | otherwise = x : uniqueChars xs

        elem' :: Char -> String -> Bool
        elem' _ [] = False
        elem' c (x:xs)
            | c == x = True
            | otherwise = elem' c xs

        count :: Char -> String -> Int
        count _ [] = 0
        count c (x:xs)
            | c == x = 1 + count c xs
            | otherwise = count c xs

-- 2
sfq :: String -> (String, Int)
sfq s = let
    words' = words (filter (`notElem` ".,!?") s)
    counts = [(word, length (filter (== word) words')) | word <- words']
    mostFrequent = maximumBy (compare `on` snd) counts
    in mostFrequent

-- 3

maximumBy :: (a -> a -> Ordering) -> [a] -> a
maximumBy _ [x] = x
maximumBy cmp (x:xs) = let
    maxRest = maximumBy cmp xs
 in if cmp x maxRest == GT then x else maxRest

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g x y = f (g x) (g y)