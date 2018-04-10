
somaNaturais :: Int -> Int
somaNaturais 0 = 0
somaNaturais a = a + somaNaturais(a-1)

maxInt :: Int -> Int -> Int
maxInt a b 
    | (a > b)   = a
    | otherwise = b