maiorDeIdade :: Int -> Bool
maiorDeIdade a = (a >= 18)

quadrado :: Int -> Int
quadrado x = x * x

mini ::Int -> Int -> Int -- Função que retornao menor valor
mini a b
    | a <= b    = a
    | otherwise = b

todosIguais :: Int -> Int -> Int -> Bool
todosIguais a b c = (a == b) && (b == c)



quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais a b c
    | (a == b) && (b == c)                  = 3
    | (a == b) || (a == c) || (b == c)      = 2
    | otherwise                             = 0


todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes a b c = (a /= b) && (a /= c) && (b /= c)

quantosSaoIguaisHipster :: Int -> Int -> Int -> Int
quantosSaoIguaisHipster a b c
    | todosIguais a b c     = 3
    | todosDiferentes a b c = 0
    | otherwise             = 2


quadradoDoQuadrado :: Int -> Int
quadradoDoQuadrado a = quadrado (quadrado a)

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

vendasTotal :: Int -> Int
vendasTotal 0 = vendas 0
vendasTotal a = vendas a + vendasTotal (a-1)

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

tamanho ::[Int] -> Int
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
concatLista :: [[Int]] ->[Int]
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

-- 18 -- Falta pra n < 0

rotateAtPosition :: Int -> [a] -> [a]
rotateAtPosition n [] = []
rotateAtPosition 0 (x:xs) = xs ++ [x]
rotateAtPosition n x = (lastChunk n x) ++ (firstChunk n x)


-------- Lista 6 23/04 -----------


