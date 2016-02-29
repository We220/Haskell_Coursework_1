
------------------------- State

type Variable = String

type State = [(Variable,Integer)]


empty :: State
empty = []


get :: State -> Variable -> Integer
get [] _ = 0
get ((x,n):xs) y | x == y    = n
                 | otherwise = get xs y


set :: Variable -> Integer -> State -> State
set x n s = (x,n) : del x s
  where
    del _ [] = []
    del y (x:xs) | fst x == y = xs
                 | otherwise  = x : del y xs


------------------------- Input-Output streams

type Stream = [Integer]

------------------------- Part 2

data Aexp = Num Integer
          | Var Variable
          | Aexp :+: Aexp
          | Aexp :-: Aexp
          | Aexp :*: Aexp
          | Next

evalA :: Aexp -> State -> Stream -> (Integer,Stream)
evalA (Num n)   _ st = (n,st)
evalA (Var v)   s st = (get s v,st)
evalA (a :+: b) s st = ((x + y),st2) where (x,st1) = evalA a s st
                                           (y,st2) = evalA b s st1 
evalA (a :*: b) s st = ((x * y),st2) where (x,st1) = evalA a s st
                                           (y,st2) = evalA b s st1 
evalA (a :-: b) s st = ((x - y),st2) where (x,st1) = evalA a s st
                                           (y,st2) = evalA b s st1 
evalA Next _ (a:as) = (a,as)

as::Stream
as = [1,2,3,4,5,6,7,8,9,10]

--a1 :: Aexp
a1 = undefined

--a2 :: Aexp
a2 = undefined



------------------------- Part 3

data Bexp = Boolean Bool
          | Aexp :==: Aexp
          | Aexp :<=: Aexp
          | Neg Bexp
          | Bexp :&: Bexp
          | Bexp :|: Bexp
          | Bexp :&&: Bexp
          | Bexp :||: Bexp

evalB :: Bexp -> State -> Stream -> (Bool,Stream)
evalB (Boolean b) _ st = (b,st)
evalB (a :==: b)  s st = ((x == y),st2) where (x,st1) = evalA a s st 
                                              (y,st2) = evalA b s st1
evalB (a :<=: b)  s st = ((x <= y),st2) where (x,st1) = evalA a s st 
                                              (y,st2) = evalA b s st1
evalB (Neg b)     s st = ((not x),st1)  where (x,st1) = evalB b s st
evalB (a :&: b)   s st = ((x && y),st2) where (x,st1) = evalB a s st 
                                              (y,st2) = evalB b s st1
evalB (a :|: b)   s st = ((x || y),st2) where (x,st1) = evalB a s st 
                                              (y,st2) = evalB b s st1
evalB (a :&&: b)  s st | evalB a s st = evalB b s st
                       | otherwise = (False,st)
evalB (a :||: b)  s st | evalB a s st = (True,st)
                       | otherwise = evalB b s st


b1 = undefined
b2 = undefined
bs = undefined


------------------------- Part 4
{-
data Comm = Skip
          | Variable :=: Aexp
          | Comm :>: Comm
          | If Bexp Comm Comm
          | While Bexp Comm
          | Print Aexp

evalC :: Comm -> State -> (State,Stream)
evalC Skip        s = (s,[])
evalC (v :=: a)   s = (set v x s,[])        where x = evalA a s 
evalC (c :>: d)   s = (u, xs ++ ys)         where (t,xs) = evalC c s
                                                  (u,ys) = evalC d t
evalC (If b c d)  s | evalB b s = evalC c s
                    | otherwise = evalC d s
evalC (While b c) s | evalB b s = (u, ys ++ zs)
                    | otherwise = (s,[])    where (t,ys) = evalC c s
                                                  (u,zs) = evalC (While b c) t
evalC (Print a)   s = (s, [evalA a s])  


{-

factorial :: Comm
factorial = ("x" :=: Next)  :>:
            ("y" :=: Num 1) :>: 
            While (Num 1 :<=: Var "x") (
                ("y" :=: (Var "x" :*: Var "y")) :>:
                ("x"  :=: (Var "x" :-: Num 1))
            ) :>:
            Print (Var "y")

runFactorial :: Stream -> Stream
runFactorial a = out 
  where 
    (_,out) = evalC factorial empty a

-}


------------------------- Part 1

c1 = (While ((Var "x") :==: (Num 0)) (Print (Num 0)))
c2 = ("x" :=: Num 0) :>: 
     While (Num 0 :<=: Var "x") 
           (("x" :=: (Var "x" :+: Num 1)) :>:
            Print (Var "x"))


------------------------- Part 5

-- data Chain =
-}