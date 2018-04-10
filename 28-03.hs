
idade :: Int
idade =17

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


-------------- Lista 3 --------------

