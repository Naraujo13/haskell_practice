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
    | otherwise  = 0


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
    | (a == 0)    = 5
    | (a == 1)    = 20
    | (a == 2)    = 40
    | (a == 3)    = 25
    | (a == 4)    = 0
    | (a == 5)    = 10
    | (a == 6)    = 15
    | (a == 7)    = 0
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
    | (m == n)                      = n
    | (vendas n == maiorVendaHipster m n)    = n
    | otherwise                     = maxVendaHipster m (n-1)

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
    | (n <= 0)   = 1
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
    | (a <= b && b <= c) = (a, b, c)
    | (a > b) = ordenaTupla (b, a, c)
    | (b > c) = ordenaTupla (a, c, b) 

-- 5

isZeroVendas :: Int -> (Int, Bool)
isZeroVendas n 
    | (vendas n == 0) = (vendas n, True)
    | otherwise = (vendas n, False)


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