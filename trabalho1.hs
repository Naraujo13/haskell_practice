------------------- Trabalho 1 -------------------

------------------------ 1 ------------------------
----- Justificar um texto segundo maior linha -----
---------- Última linha não justifica -------------

-- justifica :: String -> String
-- justifica [] = []
-- justifica x = (separaLinhas x)

-- --- Justifica lista de linhas ---
-- justificaLinhas :: [String] -> [String]
-- justificaLinhas [] = []
-- justificaLinhas (x:xs) = justificaLinha x : justificaLinhas xs

--- Justifica uma linha ---
--- Linha -> Tamanho Alvo -> Linha Justificada ---
justificaLinha :: String -> Int -> String
justificaLinha [] n = []
justificaLinha x n = insereEspacos (fst(numeroEspacosPorPalavra x n)) x

--- Separa o texto por linhas ---
separaLinhas :: String -> [String]
separaLinhas [] = []
separaLinhas x = (takeWhile (/='\n') x) : separaLinhas (tail (dropWhile (/='\n') x))

--- Retorna o tamanho da maior linha ---
tamanhoMaiorLinha :: [String]-> Int
tamanhoMaiorLinha [] = 0
tamanhoMaiorLinha (x:xs) = max (length x) (tamanhoMaiorLinha xs)

--- Separa o texto em palavras ---
separaPalavras :: String -> [String]
separaPalavras [] = []
separaPalavras x = (takeWhile (/=' ') x) : separaPalavras (tail (dropWhile (/=' ') x))

--- Número de Palavras ---
numeroPalavras :: String -> Int
numeroPalavras [] = 0
numeroPalavras x = length (separaPalavras x)

--- Calcula Número de Espaços a ser inserido entre palavras ---
--- Texto -> TamanhoAlvo -> Numero Espacos por palavra
numeroEspacosPorPalavra :: String -> Int -> (Int, Int)
numeroEspacosPorPalavra [] n = (0, 0)
numeroEspacosPorPalavra x n =  (div n (numeroPalavras x - 1), rem n (numeroPalavras x - 1))

--- Retorna string com número desejado de espaços ---
getEspacos :: Int -> String
getEspacos 0 = ""
getEspacos n = " " ++ getEspacos (n-1)

--- Adiciona N espaços a string X
putEspacos :: String -> Int -> String
putEspacos x 0 = x
putEspacos [] n = error "Lista pequena"
putEspacos (x:xs) n
  | (x == ' ')  = "  " ++ (putEspacos xs (n-1))
  | otherwise   = [x] ++ putEspacos xs (n)

--- Adiciona N espaços entre cada palavra
insereEspacos :: Int -> String -> String
insereEspacos 0 x = x
insereEspacos n [] = []
insereEspacos n (x:xs)
  | (x == ' ')  = getEspacos (n+1) ++ insereEspacos n xs
  | otherwise   = [x] ++ insereEspacos n xs


