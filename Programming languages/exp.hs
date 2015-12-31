{- Algebraic data types in Haskell -}


data Typ = NumT
         | BoolT
 deriving (Show,Eq)

data Val = NumV Int
         | BoolV Bool
 deriving Show

data Exp = NumE Int
         | VarE String
         | LetE String Exp Exp
         | AddE Exp Exp
         | LtE Exp Exp
         | IfE Exp Exp Exp

{- Environments represented as functions.
   (A solution to Homework 9 problem 4.)  -}

type Env a = String -> a

extendEnv :: Env a -> String -> a -> Env a
extendEnv env k v k' = if k == k' then v else env k' 

lookupEnv :: Env a -> String -> a
lookupEnv env k = env k

emptyEnv :: Env a
emptyEnv k = error "not found"


{- Interpreter. 
   Note: this just fails with a pattern match exception
   if a type-inconsistent operation is attempted.
-}

eval :: Env Val -> Exp -> Val
eval env e = 
  case e of
    NumE n -> NumV n
    VarE x -> lookupEnv env x
    LetE x e1 e2 -> 
      let v1 = eval env e1 in
      eval (extendEnv env x v1) e2
    AddE e1 e2 -> 
      let NumV n1 = eval env e1 
          NumV n2 = eval env e2 in
      NumV (n1 + n2)
    LtE e1 e2 -> 
      let NumV n1 = eval env e1 
          NumV n2 = eval env e2 in
      BoolV (n1 < n2)
    IfE e1 e2 e3 -> 
      let BoolV b1 = eval env e1 in
      if b1 then eval env e2 else eval env e3

{- Checker.
   Note: this just fails with a "bad type" exception 
   if the program does not type-check. -}

check :: Env Typ -> Exp -> Typ
check env e =
  case e of
    NumE n -> NumT
    VarE x -> lookupEnv env x
    LetE x e1 e2 -> 
      let t1 = check env e1 in
      check (extendEnv env x t1) e2
    AddE e1 e2 -> 
      let t1 = check env e1 
          t2 = check env e2 in
      if t1 == NumT && t2 == NumT then NumT else error "bad type"
    LtE e1 e2 -> 
      let t1 = check env e1 
          t2 = check env e2 in
      if t1 == NumT && t2 == NumT then BoolT else error "bad type"
    IfE e1 e2 e3 -> 
      let t1 = check env e1 
          t2 = check env e2
          t3 = check env e3 in
      if t1 == BoolT && t2 == t3 then t2 else error "bad type"
