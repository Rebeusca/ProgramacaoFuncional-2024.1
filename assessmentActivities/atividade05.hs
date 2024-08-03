atividade = "05"
nome = "Rebeca Amorim Penha"
matricula = ""

-- 1
bis :: Int -> Bool 
bis ano
    | mod ano 400 == 0 = True
    | mod ano 4 == 0 && mod ano 100 /= 0 = True
    | otherwise = False

-- 2
temp :: Float -> Char -> Char -> Float
temp t unidadeAtual unidadeDesejada
    | unidadeAtual == 'C' && unidadeDesejada == 'F' = t * (9 / 5) + 32
    | unidadeAtual == 'C' && unidadeDesejada == 'K' = t + 273.15
    | unidadeAtual == 'F' && unidadeDesejada == 'C' = (t - 32) * (5 / 9)
    | unidadeAtual == 'F' && unidadeDesejada == 'K' = (t - 32) * (5 / 9) + 273.15
    | unidadeAtual == 'K' && unidadeDesejada == 'C' = t - 273.15
    | unidadeAtual == 'K' && unidadeDesejada == 'F' = (t - 273.15) * (9 / 5) + 32
    | unidadeAtual == 'C' && unidadeDesejada == 'C' = t
    | unidadeAtual == 'F' && unidadeDesejada == 'F' = t
    | unidadeAtual == 'K' && unidadeDesejada == 'K' = t
    | otherwise = error "Unidades de temperatura inválidas"

-- 3
coin :: String -> [(Char, Float)] -> Float 
coin s m = sum [v | (c, v) <- m, c <- s]