------------------- Trabalho 1 -------------------

------------------------ 1 ------------------------
----- Justificar um texto segundo maior linha -----
---------- Última linha não justifica -------------

texto_exemplo_1 = "RUBIÃO fitava a enseada -- eram oito horas da manhã.\nQuem o visse com os polegares metidos no cordão do chambre à janela de uma\ngrande casa de Botafogo cuidaria que ele admirava aquele pedaço de água\nquieta mas em verdade vos digo que pensava em outra coisa.\nCotejava o passado com o presente. Que era há um ano?\nProfessor. Que é agora? Capitalista! Olha para si para as chinelas\n(umas chinelas de Túnis que lhe deu recente amigo Cristiano Palha) para a casa\npara o jardim para a enseada para os morros e para o céu e tudo desde as chinelas\naté o céu tudo entra na mesma sensação de propriedade."
texto_exemplo_2 = "Animation in user interfaces has been an issue of hot debates for several years\nso far. It’s especially active in the domain of conceptual animation for user\ninterfaces. It offers creative experiments and pushes the limits of UI motion.\nHere in Tubik we have already shared articles about UI animation and its\nbenefits for apps and website. Today, let’s continue the theme considering\nhow conceptual animation influences product success together with Tubik\nmotion designer Kirill Yerokhin."
texto_exemplo_3 = "Conceptual animation is a field of concept art.\nIt is a piece of motion design that is created to\nconvey a particular idea before it is put into a real\nproduct. In user interfaces design, conceptual\nanimation may be found in various concepts for\ninteractions, transitions, manipulations with controls,\nanimation marking the feedback from the system etc.\nMotion designers use a variety of tools among which we\ncould mention Adobe After Effects, Principle, Figma and\nInVision."
texto_exemplo_4 = "In fact, creation and research of something innovative start from a concept in practically any industry or creative field.\nLook at automotive industry or architecture, remember how new art directions appeared and developed in history. Whatever is\nthe sphere, the attitude to concepts will show two opposites from “that’s just a fantasy that has nothing to do with real\nlife” to “why not…” Both variants are viable.\nStill, for better or worse, concepts from the power that makes progress possible.\nThe same situation is observed in the domain of UI animation. Most animations which today are taken for granted as an integral\npart of our interfaces were a kind of “unreal” concepts not so long ago. In the age of flat design, when shapes and colors\nare striving for simplicity and cleanness, animation becomes the proven way to make the apps\nand solutions stand out in terms of tense competition."

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
justificaLinha x n =
  let
    espacos = numeroEspacosPorPalavra x ( n - (length x))
  in
    putEspacos (insereEspacos (fst(espacos)) x) (snd(espacos))

--- Separa o texto por linhas ---
separaLinhas :: String -> [String]
separaLinhas [] = []
separaLinhas x =
  let
    y = (dropWhile (/='\n') x)
    separa
      | y /= []   = (takeWhile (/='\n') x) : separaLinhas (tail y)
      | otherwise = (takeWhile (/='\n') x) : []
  in
    separa

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


