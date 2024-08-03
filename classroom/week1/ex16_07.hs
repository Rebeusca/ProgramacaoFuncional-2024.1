-- Aula 16/07/2024 - Terça-feira;
-- Guardas.

trianguloExiste :: Float -> Float -> Float -> Bool
trianguloExiste a b c = (a > b + c) && (a < abs b - c) 

heron :: Float -> Float -> Float -> Float
heron a b c = sqrt x
    where sP = (a + b + c) / 2
          x = sP * (sP - a) * (sP - b) * (sP - c)

areaTriangulo :: Float -> Float -> Float -> String -> Float
areaTriangulo a b c tipo
    | tipo == "baseAltura" && a * b > 0 = (a * b) / 2 
    | tipo == "lados" && a > 0 && b > 0 && c > 0 && trianguloExiste a b c = sqrt(s * (s - a) * (s - b) * (s - c))
    | tipo == "ladoAngulo" && a > 0 && b > 0 && sin c /= 0 = (a * b * sin c) / 2
    | otherwise = error "Tipo inválido!"
    where s = (a + b + c) / 2

-----------------------------------------------------------------------------------------------------------------------

-- Faça uma função que usa guards que recebe um número:
-- se n > 1, retorna n;
-- se n > 2, retorna n**2
-- se n > 3, retorna n**3
-- se n > 4, retorna n**4
x01 :: Int -> Int
x01 n
    | n > 4 = n ^ 4
    | n > 3 = n ^ 3
    | n > 2 = n ^ 2
    | n > 1 = n
    | otherwise = 0
-- O compilador não verifica a possibilidade de múltiplas entradas.