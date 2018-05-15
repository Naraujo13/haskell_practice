------------------- Trabalho 1 -------------------

------------------------ 1 ------------------------
----- Justificar um texto segundo maior linha -----
---------- Última linha não justifica -------------

texto_exemplo = "RUBIÃO fitava a enseada -- eram oito horas da manhã.\nQuem o visse com os polegares metidos no cordão do chambre à janela de uma\ngrande casa de Botafogo cuidaria que ele admirava aquele pedaço de água\nquieta mas em verdade vos digo que pensava em outra coisa.\nCotejava o passado com o presente. Que era há um ano?\nProfessor. Que é agora? Capitalista! Olha para si para as chinelas\n(umas chinelas de Túnis que lhe deu recente amigo Cristiano Palha) para a casa\npara o jardim para a enseada para os morros e para o céu e tudo desde as chinelas\naté o céu tudo entra na mesma sensação de propriedade.\n"

justifica :: String -> String
justifica [] = []
justifica x =
  let
    array_linhas = (separaLinhas x)
  in
    (justificaLinhas (init array_linhas) (tamanhoMaiorLinha array_linhas)) ++ (last(array_linhas)) ++ "\n"

--- [Lista de Linhas] -> TamanhoMaiorLinha -> TextoJustificado
-- --- Justifica lista de linhas ---
justificaLinhas :: [String] -> Int -> String
justificaLinhas [] n = []
justificaLinhas (x:xs) n = (justificaLinha x n) ++ "\n" ++ (justificaLinhas xs n)

--- Justifica uma linha ---
--- Linha -> Tamanho Alvo -> Linha Justificada ---
justificaLinha :: String -> Int -> String
justificaLinha [] n = []
justificaLinha x n = insereEspacos (fst(numeroEspacosPorPalavra x ( n - (length x)))) x

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
separaPalavras x =
  let
      y = (dropWhile(/=' ') x)
      notEmpty
        | (y /= []) = (takeWhile (/=' ') x) : separaPalavras (tail (dropWhile (/=' ') x))
        | otherwise = (takeWhile (/=' ') x) : []
  in
      notEmpty

--- Número de Palavras ---
numeroPalavras :: String -> Int
numeroPalavras [] = 0
numeroPalavras x = length (separaPalavras x)

--- Calcula Número de Espaços a ser inserido entre palavras ---
--- Texto -> Numero de Espaços para inserir -> Numero Espacos por palavra
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


