-- Aula 22/07/24 - Segunda - feira;
-- Casamento de padrão.

-- 
-- up :: Char -> (a, b) -> Char
up ch
  | ls == [] = ch
  | otherwise = (ls !! 0)
  where minus = ['a'..'z']
        maius = ['A'..'Z']
        f = zip minus maius
        ls = [ b | (a,b) <- f, a == ch]

-- Faça uma função que receba uma string e retorne-a em seu modo capitalize.
-- capWord :: String -> String
-- capWord :: [Char] -> String
capWord [] = ""
capWord (x:s) = (up x):s

-- Dada uma string usa o words e separa nas palavas e em cada um usando capWord
-- cap :: String -> String
cap [] = ""
cap s = unwords capws
  where ws = words s
        capws = [capWord w | w <- ws] 
