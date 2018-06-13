
-------------- Lista 1 --------------

--- 1

maiorDeIdade :: Num a =>  Ord a => a -> Bool
maiorDeIdade a = (a >= 18)

--- 2

quadrado :: Num a => a -> a
quadrado x = x * x

mini :: Ord a => a -> a -> a
mini a b
    | a <= b    = a
    | otherwise = b

--- 3

todosIguais :: Eq a => a -> a -> a -> Bool
todosIguais a b c = (a == b) && (b == c)

--- 4

quantosSaoIguais :: Eq a => a -> a -> a -> Int
quantosSaoIguais a b c
    | (a == b) && (b == c)                  = 3
    | (a == b) || (a == c) || (b == c)      = 2
    | otherwise                             = 0

--- 5

todosDiferentes :: Eq a => a -> a -> a -> Bool
todosDiferentes a b c = (a /= b) && (a /= c) && (b /= c)

--- 6

quantosSaoIguaisHipster :: Eq a => a -> a -> a -> Int
quantosSaoIguaisHipster a b c
    | todosIguais a b c     = 3
    | todosDiferentes a b c = 0
    | otherwise             = 2

--- 7

quadradoDoQuadrado :: Num a => a -> a
quadradoDoQuadrado a = quadrado (quadrado a)

--- 8

vendas :: Int -> Int
vendas a
    | (a == 0)  = 5
    | (a == 1)  = 20
    | (a == 2)  = 40
    | (a == 3)  = 25
    | (a == 4)  = 0
    | (a == 5)  = 10
    | (a == 6)  = 15
    | (a == 7)  = 0
    | otherwise = 30

--- 9

vendasTotal :: Int -> Int
vendasTotal 0 = vendas 0
vendasTotal a = vendas a + vendasTotal (a-1)

--- 10

divisao :: Float -> Float -> Float
divisao a b = a / b


-------------- Lista 2 --------------

-- Exemplo de Aula

somaNaturais :: Int -> Int
somaNaturais 0 = 0
somaNaturais a = a + somaNaturais(a-1)

-- 1

maxInt :: Int -> Int -> Int
maxInt a b
    | (a > b)   = a
    | otherwise = b

-- 2

maiorVenda :: Int -> Int
maiorVenda 0 = vendas 0
maiorVenda n = maxInt (vendas n) (maiorVenda (n-1))

-- 3
maxVenda :: Int -> Int
maxVenda 0 = 0
maxVenda n
    | (vendas n == maiorVenda n)    = n
    | otherwise                     = maxVenda (n-1)

-- 4
zeroVendas :: Int -> Int
zeroVendas 0
    | (vendas 0 == 0)   = 0
    | otherwise         = -1
zeroVendas n
    | (vendas n == 0)   = n
    | otherwise         = zeroVendas(n-1)

-- 5

igualVendas :: Int -> Int -> Int
igualVendas s 0
    | (vendas 0 == s)   = 0
    | otherwise         = -1
igualVendas s n
    | (vendas n == s)   = n
    | otherwise         = igualVendas s (n-1)

-- 6
-- A questão 6 é idiota.

-- 7

maiorVendaHipster :: Int -> Int -> Int
maiorVendaHipster m n
    | (m == n)  = vendas m
    | otherwise = maxInt (vendas n) (maiorVendaHipster m (n-1))

maxVendaHipster :: Int -> Int -> Int
maxVendaHipster m n
    | (m == n)                              = n
    | (vendas n == maiorVendaHipster m n)   = n
    | otherwise                             = maxVendaHipster m (n-1)

zeroVendasHipster :: Int -> Int -> Int
zeroVendasHipster m n
    | (vendas n == 0)   = n
    | (m == n)          = -1
    | otherwise         = zeroVendasHipster m (n-1)

igualVendasHipster :: Int -> Int -> Int -> Int
igualVendasHipster s m n
    | (vendas n == s)   = n
    | (m == n)          = -1
    | otherwise         = igualVendasHipster s m (n-1)

-- 8

fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n-1)

-- Error "string"
-- 9
fatorialHipster :: Int -> Int -> Int
fatorialHipster m n
    | (m > n)   = error "Erro. Segundo parâmetro deve ser maior que o primeiro."
    | (n <= 0)  = 1
    | (m == n)  = n
    | otherwise = n * fatorialHipster m (n-1)

-- 10

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)


-------------- Lista 3 10/04 --------------

-- Exemplo de Aula 1

type Pessoa = (String, String, Int)
joao :: Pessoa
joao = ("João Final de Semana", "666-666", 22)

nome :: Pessoa -> String
nome (n,t,i) = n

telefone :: Pessoa -> String
telefone (n,t,i) = t

idade :: Pessoa -> Int
idade (n,t,i) = i

-- Exemplo de Aula 2

tabela :: Int -> String
tabela 0 = "Semana 0\t" ++ show(vendas 0) ++ "\n"
tabela a = tabela (a-1) ++ "Semana " ++ show(a) ++ "\t" ++ show(vendas a) ++ "\n"


-- 1

adicionaTupla :: (Int, Int) -> Int
adicionaTupla (a, b) = (a + b)

-- 2

shift :: ((Int, Int), Int) -> (Int, (Int, Int))
shift ((a, b), c) = (a, (b, c))

-- 3

minEmax :: Int -> Int -> Int -> (Int, Int)
minEmax a b c = ((min a (min b c)), (max a (max b c)))

-- 4

ordenaTupla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTupla (a, b, c)
    | (a <= b && b <= c)    = (a, b, c)
    | (a > b)               = ordenaTupla (b, a, c)
    | (b > c)               = ordenaTupla (a, c, b)

-- 5

isZeroVendas :: Int -> (Int, Bool)
isZeroVendas n
    | (vendas n == 0)   = (vendas n, True)
    | otherwise         = (vendas n, False)


-- 6

type Livro = (String, String, Int)

titulo :: Livro -> String
titulo (t,a,i) = t

autor :: Livro -> String
autor (t,a,i) = a

isbn :: Livro -> Int
isbn (n,t,i) = i

sbcup :: Livro
sbcup = ("Metro 2033", "Dmitry Glutoevsky", 38437635)

-------- Lista 4 11/04 -----------

-- Exemplo 1 de Aula

somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (a : x) = a + somaLista x

-- Exemplo 2 de Aula

retira :: Int -> [a] -> [a]
retira 0 (x:xs) = x:xs
retira a [] = []
retira a (x:xs) = retira (a-1) (xs)

-- 1

dobraLista :: [Int] -> [Int]
dobraLista [] = []
dobraLista (a : x) = (2*a) : dobraLista x

-- 2

tamanho ::[a] -> Int
tamanho [] = 0
tamanho (a : x) = 1 + tamanho x

-- 3

produtoLista :: [Int] -> Int
produtoLista [a] = a
produtoLista (a : x) = a * produtoLista x

-- 4

andLista :: [Bool] -> Bool
andLista [a] = True && a
andLista (a : x) = a && andLista x

-- 5
concatLista :: [[a]] ->[a]
concatLista [] = []
concatLista ([]:xss) = concatLista xss
concatLista ((x:xs):xss) = x : concatLista (xs:xss)


-- 6
inverteLista :: [Int] -> [Int]
inverteLista [] = []
inverteLista (a : x) = inverteLista x ++ [a]


-------- Lista 5 17/04 -----------

-- Exemplo 1 - Insertion Sort

iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

ins :: Int -> [Int] -> [Int]
ins n [] = [n]
ins n (x:xs) = if (n > x) then (x : ins n xs) else (n : x : xs)

-- Exemplo 2 - Quick Sort

qSort :: [Int] -> [Int]
qSort [] = []
qSort (x:xs) = qSort(menores x xs) ++ [x] ++ qSort(maioresOuIguais x xs)

menores :: Int -> [Int] -> [Int]
menores n [] = []
menores n (x:xs) = if (x < n) then (x : menores n xs) else (menores n xs)

maioresOuIguais :: Int -> [Int] -> [Int]
maioresOuIguais n [] = []
maioresOuIguais n (x:xs) = if (x >= n) then (x : maioresOuIguais n xs) else (maioresOuIguais n xs)

-- Exemplo 3 - Insertion Sort sem repetições
iUniqueSort :: [Int] -> [Int]
iUniqueSort [] = []
iUniqueSort (x:xs) = uIns x (iUniqueSort xs)

uIns :: Int -> [Int] -> [Int]
uIns n [] = [n]
uIns n (x:xs) = if (n > x) then (x : uIns n xs) else if (n == x) then (x : xs) else (n : x : xs)

-- Exemplo 4 - Retornar Máximo e Mínimo de uma Lista

minMaxList :: [Int] -> (Int, Int)
minMaxList [] = (0, 0)
minMaxList x = (head(qSort x), last(qSort x))

-- Exemplo 5 - QuickSort decrescente

iSortDec :: [Int] -> [Int]
iSortDec [] = []
iSortDec (x:xs) = insDec x (iSortDec xs)

insDec :: Int -> [Int] -> [Int]
insDec n [] = [n]
insDec n (x:xs) = if (n < x) then (x : insDec n xs) else (n : x : xs)


-- 1

exists :: Int -> [Int] -> Bool
exists n [] = False
exists n (x:xs) = if (n == x) then True else (exists n xs)


-- 2

existsNum :: Int -> [Int] -> Int
existsNum n [] = 0
existsNum n (x:xs) = if (n == x) then (1 + existsNum n xs) else (existsNum n xs)

-- 3 O três é idiota

-- 4

dropDuplicates :: [Int] -> [Int]
dropDuplicates [] = []
dropDuplicates (x:xs) = if (existsNum x xs > 0) then (dropDuplicates (dropElement x xs)) else (x : dropDuplicates xs)

dropElement :: Eq a => a -> [a] -> [a]
dropElement n [] = []
dropElement n (x:xs) = if (n == x) then (dropElement n xs) else (x : dropElement n xs)

-- 5

-- existsHipster :: Int -> [Int] -> Bool
-- existsHipster n [] = False
-- existsHipster n x = iSort


-------- Lista Extra 18/04 -----------

-- 1
retornaUltimo :: [a] -> a
retornaUltimo [] = error ("Empty List")
retornaUltimo(x:[]) = x
retornaUltimo (x:xs) = retornaUltimo(xs)

-- 2

getPosition :: Int -> [a] -> a
getPosition n [] = error ("First argument is larger than list size")
getPosition 0 (x:xs) = x
getPosition n (x:xs) = getPosition (n-1) xs

-- 3

getUntil :: Int -> [a] -> [a]
getUntil n [] = []
getUntil 0 (x:xs) = [x]
getUntil n (x:xs) = x : getUntil (n-1) xs

-- 4

removeUntil :: Int -> [a] -> [a]
removeUntil n [] = []
removeUntil 0 (x:xs) = xs
removeUntil n (x:xs) = removeUntil (n-1) xs

-- 5

listMean :: [Int] -> Float
listMean [] = error ("Empty List")
listMean x = fromIntegral (somaLista x) / fromIntegral (length x)

-- 6

getTop :: Int -> [Int] -> [Int]
getTop n [] = []
getTop 0 x = []
getTop n x = getUntil (n-1) (iSortDec x)

-- 7

howManyAreBigger :: Int -> [Int] -> Int
howManyAreBigger n [] = 0
howManyAreBigger n (x:xs)
    | (x > n)   = 1 + howManyAreBigger n xs
    | otherwise = howManyAreBigger n xs

-- 8

concatena :: [a] -> [a] -> [a]
concatena [] [] = []
concatena [] (x:xs) = x : concatena [] xs
concatena (x:xs) [] = x : concatena xs []
concatena (x:xs) y = x : concatena xs y

-- 9

intercala :: [a] -> [a] -> [a]
intercala x [] = x
intercala [] x = x
intercala (x:xs) (y:ys) = x : y : intercala xs ys

-- 10

compress :: Eq a =>  [a] -> [a]
compress [] = []
compress (x:xs) = x : compress (removeElementFromBeggining x xs)
    where
        removeElementFromBeggining n [] = []
        removeElementFromBeggining n (x:xs)
            | (n==x)    = removeElementFromBeggining n xs
            | otherwise = (x:xs)

-- 11

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = ([x] ++ (getEqualsNext x xs)) : pack (removeFromBeggining x xs)
    where
        getEqualsNext n [] = []
        getEqualsNext n (x:xs)
            | (n == x)  = x : getEqualsNext n xs
            | otherwise = []
        removeFromBeggining n [] = []
        removeFromBeggining n (x:xs)
            | (n == x)    = removeFromBeggining n xs
            | otherwise = (x:xs)

-- 12

encode :: Eq a => [a] -> [(a, Integer)]
encode [] = []
encode (x:xs) = (x, (countFromBeggining x (x:xs))) : (encode (dropElement x xs))
    where
        countFromBeggining n []     = 0
        countFromBeggining n (x:xs)
            | (n == x)  = 1 + (countFromBeggining n xs)
            | otherwise = 0


-- 13

duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x : x : duplicate xs


-- 14

replicateElements :: Int -> [a] -> [a]
replicateElements n [] = []
replicateElements 0 x = []
replicateElements n (x:xs) = (repeatElement n x) ++ (replicateElements n xs)

repeatElement :: Int -> a -> [a]
repeatElement 0 x = []
repeatElement n x = [x] ++ repeatElement (n-1) x

-- 15

dropElementAtPosition :: Int -> [a] -> [a]
dropElementAtPosition n [] = []
dropElementAtPosition 0 (x:xs) = xs
dropElementAtPosition n (x:xs) = x : dropElementAtPosition (n-1) xs

-- 16

splitAtPosition :: Int -> [a] -> ([a], [a])
splitAtPosition n [] = ([], [])
splitAtPosition n x = (firstChunk n x, lastChunk n x)

firstChunk :: Int -> [a] -> [a]
firstChunk n [] = []
firstChunk 0 (x:xs) = [x]
firstChunk n (x:xs) = [x] ++ firstChunk (n-1) xs

lastChunk :: Int -> [a] -> [a]
lastChunk n [] = []
lastChunk 0 (x:xs) = xs
lastChunk n (x:xs) = lastChunk (n-1) xs

-- 17

slice :: Int -> Int -> [a] -> [a]
slice n m [] = []
slice 0 0 (x:xs) = [x]
slice 0 m (x:xs) = x : (slice 0 (m-1) xs)
slice n m (x:xs) = slice (n-1) (m-1) xs

-- 18

rotateAtPosition :: Int -> [a] -> [a]
rotateAtPosition n [] = []
rotateAtPosition 0 (x:xs) = xs ++ [x]
rotateAtPosition n x
    | (n >= 0)  = (lastChunk n x) ++ (firstChunk n x)
    | otherwise = (lastChunk ((length x) - 1 + n) x) ++ (firstChunk ((length x) + n) x)

-- 19

removeAt :: Int -> [a] -> (a, [a])
removeAt n [] = error "First argument is bigger than list size"
removeAt 0 (x:xs) = (x, xs)
removeAt n x = (getPosition n x, deleteAtPosition n x)

deleteAtPosition :: Int -> [a] -> [a]
deleteAtPosition n [] = []
deleteAtPosition 0 (x:xs) = xs
deleteAtPosition n (x:xs) = x : deleteAtPosition (n-1) xs

-------- Lista 6 23/04 -----------

-- 1

sumTriples :: [(Int, Int, Int)] -> Int
sumTriples [] = 0
sumTriples ((a,b,c):xs) = a + b + c + sumTriples xs

-- 2

sumTupleOfTuples :: [((Int, Int), (Int, Int))] -> Int
sumTupleOfTuples [] = 0
sumTupleOfTuples (((a,b),(c,d)):xs) = a + b + c + d + sumTupleOfTuples xs

-- 3

zipp :: [a] -> [a] -> [(a, a)]
zipp [] x = []
zipp x [] = []
zipp (x:xs) (y:ys) = (x, y) : zipp xs ys

-- 4

zipp3 :: [a] -> [a] -> [a] -> [(a, a, a)]
zipp3 [] y z = []
zipp3 x [] z = []
zipp3 x y [] = []
zipp3 (x:xs) (y:ys) (z:zs) = (x, y, z) : zipp3 xs ys zs

-- 5

unZipp :: [(a, a)] -> ([a], [a])
unZipp [] = ([], [])
unZipp x = (getListOfFirst x, getListOfLast x)
    where
        getListOfFirst [] = []
        getListOfFirst (x:xs) = fst x : getListOfFirst xs
        getListOfLast [] = []
        getListOfLast (x:xs) = snd x : getListOfLast xs



-- List 7 - Aula 02/05/18

times2 n = 2*n
times3 n = 3*n

mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt f [] = []
mapInt f (x:xs) = f x : mapInt f xs


-- 1

total :: (Int -> Int) -> Int -> Int
total f 0 = f 0
total f n = f n + total f (n-1)

-- 2

foldInt :: (Int -> Int -> Int) -> [Int] -> Int
foldInt f [] = error "Empty List"
foldInt f x = foldHipster f (reverse x)
    where
        foldHipster f [] = error "Empty List"
        foldHipster f [x] = x
        foldHipster f (x:xs) = f (foldHipster f xs) x


soma x y = (x + y)
subtracao x y = (x - y)
multiplicacao x y = (x * y)


--- Lista 8 - tipos polimorficos, como fiz em boa parte dos anteriores, pulei

--- Lista 9 - Fold e Map, reimplementação de vários exercícios com fold e map

-- 1

concatenaF :: [[a]] -> [a]
concatenaF [] = []
concatenaF (x:xs) = foldl (++) x xs

-- 2

andF :: [Bool] -> Bool
andF [] = True
andF (x:xs) = foldl (&&) x xs


--- Lista de Aula de Semântica - Tipos Algébricos


-- Exemplo 1

data Temperatura = Frio|Calor
    deriving(Eq, Show)

data Estacao = Verao|Outono|Inverno|Primavera

tempo :: Estacao -> Temperatura
tempo Verao = Calor
tempo _     = Frio


-- Exemplo 2

-- Exemplo 3
data Forma = Circulo Float | Retangulo Float Float
    deriving(Eq, Show)

redondo :: Forma -> Bool
redondo (Circulo x) = True
redondo _           = False

area :: Forma -> Float
area (Circulo x)        = pi * x * x
area (Retangulo b h)    = b * h

-- Exemplo 4
data Arvore = Folha | Nodo Int Arvore Arvore
    deriving(Eq, Show)

minhaArvore :: Arvore
minhaArvore = Nodo 10 (Nodo 14 (Nodo 1 Folha Folha) Folha) Folha

somaArvore :: Arvore -> Int
somaArvore Folha = 0
somaArvore (Nodo v n1 n2) = v + somaArvore n1 + somaArvore n2


--- Lista 5 (Semântica) - Tipos Algébricos

-- 1

dobraArvore :: Arvore -> Arvore
dobraArvore Folha = Folha
dobraArvore (Nodo v n1 n2) = Nodo (v*2) (dobraArvore n1) (dobraArvore n2)

-- 2

maxArvore :: Arvore -> Int
maxArvore Folha = 0
maxArvore (Nodo v n1 n2) = max v (max (maxArvore n1) (maxArvore n2))

-- 3

existsArvore :: Int -> Arvore -> Bool
existsArvore _ Folha = False
existsArvore n (Nodo v n1 n2) = (n == v) || (existsArvore n n1) || (existsArvore n n2)

-- 4 - Cópia da 2

-- 5

countArvore :: Int -> Arvore -> Int
countArvore _ Folha = 0
countArvore n (Nodo v n1 n2)
    | (n == v)  = 1 + (countArvore n n1) + (countArvore n n2)
    |otherwise  = (countArvore n n1) + (countArvore n n2)

-- 6

refleteArvore :: Arvore -> Arvore
refleteArvore Folha = Folha
refleteArvore (Nodo v n1 n2) = Nodo v (refleteArvore n2) (refleteArvore n1)

-- 7

arvoreToList :: Arvore -> [Int]
arvoreToList Folha = []
arvoreToList (Nodo v n1 n2) = [v] ++ (arvoreToList n1) ++ (arvoreToList n2)

-- 8

mapTree :: (Int -> Int) -> Arvore -> Arvore
mapTree _ Folha = Folha
mapTree f (Nodo v n1 n2) = Nodo (f v) (mapTree f n1) (mapTree f n2)

-- 9

data Lista a = Fim | Elemento a (Lista a)
    deriving(Eq, Show)

minhaLista :: Lista Int
minhaLista = Elemento 1 (Elemento 2 (Elemento 3 Fim))

minhaListaBool :: Lista Bool
minhaListaBool = Elemento True (Elemento False (Elemento True Fim))

tamanhoLista :: (Lista a) -> Int
tamanhoLista Fim = 0
tamanhoLista (Elemento x p) = 1 + tamanhoLista p

mapLista :: (a -> a) -> (Lista a) -> (Lista a)
mapLista _ Fim = Fim
mapLista f (Elemento x p) = Elemento (f x) (mapLista f p)


---- Exercícios 10 - Tipos Algébricos Simples

-- 1
type Titulo = String
type Artista = String
type Diretor = String
type Ano = Int

data ItemLocadora = CD Titulo Artista Ano | DVD Titulo  Diretor Ano
    deriving(Eq, Show)

getTitulo :: ItemLocadora -> String
getTitulo (CD t a y) = t
getTitulo (DVD t d y) = t

getArtista :: ItemLocadora -> String
getArtista (CD t a y) = a
getArtista _ = error "DVD não tem artista"

getDiretor :: ItemLocadora -> String
getDiretor (DVD t d y) = d
getDiretor _ = error "CD não possui diretor"

getAno :: ItemLocadora -> Int
getAno (CD t a y) = y
getAno (DVD t d y) = y

meuCD :: ItemLocadora
meuCD = CD "American Idiot" "Green Day" 2004

meuDVD :: ItemLocadora
meuDVD = DVD "Laranja Mecânica" "Esqueci o Nome" 1980

-- 2

type Nome = String
type Idade = Int
type CPF = String

data Socio = Pessoa Nome Idade CPF
    deriving(Eq, Show)

meuSocio :: Socio
meuSocio = Pessoa "Nícolas de Araujo" 21 "033.690.370-76"

meuSocio2 :: Socio
meuSocio2 = Pessoa "Eduarda Rosa" 24 "111.111.111-11"


-- 3

data ItensDisponiveis = Acervo [ItemLocadora]
    deriving(Eq, Show)

meuAcervo :: ItensDisponiveis
meuAcervo = Acervo (meuCD : meuDVD : [])

-- 4

data ItemAlugado = Aluguel ItemLocadora Socio
    deriving(Eq, Show)

meuAluguel1 :: ItemAlugado
meuAluguel1 = meuCD `Aluguel` meuSocio
meuAluguel2 :: ItemAlugado
meuAluguel2 = meuDVD `Aluguel` meuSocio2

-- 5
data ItensIndisponiveis = Indisponiveis [ItemAlugado]
    deriving(Eq, Show)

meuAcervoAlugado :: ItensIndisponiveis
meuAcervoAlugado = Indisponiveis (meuAluguel1 : meuAluguel2 : [])

-- 6
alugaItem :: Socio -> ItemLocadora -> ItemAlugado
alugaItem s i = i `Aluguel` s

-- 7
adicionaIndisponivel :: ItemAlugado -> ItensIndisponiveis -> ItensIndisponiveis
adicionaIndisponivel i (Indisponiveis x) = Indisponiveis (i:x)

alugadosPara :: ItensIndisponiveis -> Socio -> ItensIndisponiveis
alugadosPara (Indisponiveis []) s2 = Indisponiveis []
alugadosPara (Indisponiveis ((Aluguel i s1):xs)) s2
    | (s1 == s2)    = adicionaIndisponivel (Aluguel i s1) (alugadosPara (Indisponiveis xs) s2)
    | otherwise     = alugadosPara (Indisponiveis xs) s2
