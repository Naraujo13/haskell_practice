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
  | For AExp AExp CExp
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
cbigStep (For current end c, s) =
    case (fst(abigStep(current, s)) == fst(abigStep(end, s))) of
        (True) -> cbigStep(Skip, s)
        (False) -> cbigStep(Seq c (For (Som current (Num 1)) end c), s)


-- Swap
cbigStep (Swap (Var x) (Var y), s) = 
    let 
        xv = (procuraVar s x)
        yv = (procuraVar s y)
    in (Skip,  mudaVar (mudaVar s x yv) y xv)


meuEstado :: Estado
meuEstado = [("x",3), ("y",0), ("z",0)]


exemplo :: AExp
exemplo = Mul (Num 3) (Sub (Var "x") (Var "y"))

teste1 :: BExp
teste1 = (Or (And (Ig (Som (Num 3) (Num 3))  (Mul (Num 2) (Num 3))) FALSE) TRUE)
--teste2 :: BExp
--teste2 = (Ig (Som (Var "x") (Num 3))  (Mul (Num 2) (Num 3)))


--testec1 :: CExp
--testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
--		(Atrib (Var "y") (Var "z")))

--fatorial :: CExp
--fatorial = (Seq (Atrib (Var "y") (Num 1))
--                (While (Not (Ig (Var "x") (Num 1)))
--                       (Seq (Atrib (Var "y") (Mul (Var "y") (Var "x")))
--                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))



--------- Trabalho ----------------
--- 1) Completar a semântica definida no arquivo bigstep.hs
--- 3) Adicionar os seguintes comandos:
----- a) Repeat C Until B
----- b) Do C While B
----- c) For X From E1 to E2 Do C
----- d) Swap (x,y)
----- e) x,y := E1, E2