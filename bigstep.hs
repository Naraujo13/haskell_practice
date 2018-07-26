import Estado


data AExp = Num Int 
  |Var String 
  |Som AExp AExp
  |Sub AExp AExp
  |Mul AExp AExp
  deriving(Show)

data BExp = TRUE
  | FALSE
  | Not BExp
  | And BExp BExp
  | Or  BExp BExp
  | Ig  AExp AExp
  deriving(Show)

data CExp = While BExp CExp
  | If BExp CExp CExp
  | Seq CExp CExp
  | Atrib AExp AExp
  | Multiatrib AExp AExp AExp AExp
  | Repeat CExp BExp
  | Do CExp BExp
  | For AExp AExp AExp CExp
  | Swap AExp AExp
  | Skip
  deriving(Show)                


abigStep :: (AExp,Estado) -> (Int,Estado)
abigStep (Var x,s) = (procuraVar s x,s)
abigStep (Num n,s) = (n,s)
abigStep (Som e1 e2,s)  = 
    let (n1,s1) = abigStep (e1, s)
        (n2,s2) = abigStep (e2, s)
    in (n1+n2,s)
abigStep (Sub e1 e2, s) =
    let (n1, s1) = abigStep (e1, s)
        (n2, s2) = abigStep (e2, s)
    in (n1-n2, s)
abigStep (Mul e1 e2, s) =
    let (n1, s1) = abigStep (e1, s)
        (n2, s2) = abigStep (e2, s)
    in (n1*n2, s)

bbigStep :: (BExp,Estado) -> (Bool,Estado)
bbigStep (TRUE,s) = (True,s)
bbigStep (FALSE,s) = (False,s)
bbigStep (Not b,s) = 
    case bbigStep (b,s) of
        (True,_) -> (False, s)
        (False,_) -> (True, s)
bbigStep (Ig e1 e2, s) =
    let (n1, s1) = abigStep (e1, s)
        (n2, s2) = abigStep (e2, s)
    in (n1==n2, s)
bbigStep (And e1 e2, s) =
    case bbigStep (e1, s) of
        (True, _) -> bbigStep (e2, s)
        (False, _) ->  (False, s)
bbigStep (Or e1 e2, s) =
    case bbigStep (e1, s) of
        (True, _) -> (True, s)
        (False, _) ->  bbigStep (e2, s)


-- IF
cbigStep :: (CExp,Estado) -> (CExp,Estado)
cbigStep (Skip,s) = (Skip,s)
cbigStep (If b c1 c2,s) = 
    case bbigStep (b,s) of
        (True, _) -> cbigStep (c1, s)
        (False, _) -> cbigStep (c2, s)

-- Atrib        
cbigStep (Atrib (Var x) e,s) = 
    let (n, s1) = abigStep (e, s)
    in (Skip, (mudaVar s x n))

-- MultiAtrib
cbigStep (Multiatrib (Var x) (Var y) e1 e2, s) = 
    let 
        (n1, _) = abigStep (e1, s)
        (n2, _) = abigStep (e2, s)
    in (Skip, mudaVar (mudaVar s x n1) y n2)


-- Seq
cbigStep (Seq c1 c2,s)  = 
    case cbigStep (c1, s) of
        (Skip, s1) -> cbigStep (c2, s1)
        (c3, s1) -> cbigStep (c3, s1) 

-- While
cbigStep (While b c, s) = 
    case bbigStep (b, s) of
        (True, _) -> cbigStep(Seq c (While b c), s)
        (False, _) -> cbigStep(Skip, s)

-- Repeat Until
cbigStep (Repeat c b, s) =
    case bbigStep (b, s) of
        (True, _) -> cbigStep(Skip, s)
        (False, _) -> cbigStep(Seq c (Repeat c b), s)


-- Do While
cbigStep (Do c b, s) =
    case bbigStep (b, s) of
        (True, _) -> cbigStep(Seq c (Do c b), s)
        (False, _) -> cbigStep(Skip, s)  


-- For
cbigStep (For v current end c, s) =
    case (fst(abigStep((Som current (Num 1)), s)) == fst(abigStep(end, s))) of
        (True) -> (cbigStep (Atrib v (Som current (Num 1)), s))
        (False) -> cbigStep(Seq (Atrib v (Som current (Num 1))) (Seq c (For v (Som current (Num 1)) end c)), s)


-- Swap
cbigStep (Swap (Var x) (Var y), s) = 
    let 
        xv = (procuraVar s x)
        yv = (procuraVar s y)
    in (Skip,  mudaVar (mudaVar s x yv) y xv)


meuEstado :: Estado
meuEstado = [("x",3), ("y",0), ("z",0)]

exemplo :: AExp
exemplo = Som (Num 3) (Som (Var "x") (Var "y"))

exemplo2 :: AExp
exemplo2 = Sub (Num 3) (Sub (Var "x") (Var "y"))

exemplo3 :: AExp
exemplo3 = Mul (Num 3) (Som (Var "x") (Var "y"))

exemploAnd1 :: BExp
exemploAnd1 = And TRUE FALSE

exemploAnd2 :: BExp
exemploAnd2 = And (TRUE) (Or TRUE FALSE)

exemploOr1 :: BExp
exemploOr1 = Or TRUE FALSE

exemploOr2 :: BExp
exemploOr2 = Or (FALSE) (And TRUE FALSE)

exemploSeq1 :: CExp
exemploSeq1 = (Seq (Atrib (Var "z") (Num 1)) (Atrib (Var "x") (Num 2)))

exemploDo1 :: CExp
exemploDo1 = (Do (Seq(Atrib (Var "x") (Sub (Var "x") (Num 1)))(Skip)) (Not (Ig (Var "x") (Num 1))))  

exemploDo2 :: CExp
exemploDo2 = (Do (Seq(Atrib (Var "x") (Som (Var "x") (Num 1)))(Skip)) (Not (Ig (Var "x") (Num 10))))

exemploWhile1 :: CExp
exemploWhile1 = (While (Ig (Var "x") (Num 10)) (Seq(Atrib (Var "x") (Som (Var "x") (Num 1)))(Skip)))

exemploMultiAtrib :: CExp
exemploMultiAtrib = (Multiatrib (Var "x") (Var "y") (Num 88) (Num 99))

exemploRepeat1 :: CExp
exemploRepeat1 = (Repeat (Seq(Atrib (Var "x") (Som (Var "x") (Num 1)))(Skip)) (Ig (Var "x") (Num 10)))

exemploRepeat2 :: CExp
exemploRepeat2 = (Repeat (Seq(Atrib (Var "x") (Som (Var "x") (Num 1)))(Skip)) (Ig (Var "x") (Num 20)))

exemploSwap1 :: CExp
exemploSwap1 = (Swap (Var "x") (Var "y"))

exemploSwap2 :: CExp
exemploSwap2 = (Seq (Seq (Atrib (Var "x") (Num 1)) (Atrib (Var "y") (Num 2))) (Swap(Var "x")(Var "y")))

exemploFor1 :: CExp
exemploFor1 = (For (Var "x") (Num 1) (Num 5) (Atrib (Var "x")(Som (Var "x")(Num 1))))

exemploFor2 :: CExp
exemploFor2 = (For (Var "x") (Num 1) (Num 10) (Atrib (Var "x")(Som (Var "x")(Num 2))))

teste1 :: BExp
teste1 = (Ig (Som (Num 3) (Num 3))  (Mul (Num 2) (Num 3)))
teste2 :: BExp
teste2 = (Ig (Som (Var "x") (Num 3))  (Mul (Num 2) (Num 3)))


fatorial :: CExp
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Ig (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mul (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))


--------- Trabalho ----------------
--- 1) Completar a semântica definida no arquivo bigstep.hs
--- 3) Adicionar os seguintes comandos:
----- a) Repeat C Until B
----- b) Do C While B
----- c) For X From E1 to E2 Do C
----- d) Swap (x,y)
----- e) x,y := E1, E2