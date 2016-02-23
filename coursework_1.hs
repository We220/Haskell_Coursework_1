
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

evalA :: Aexp -> State -> Integer
evalA (Num n)   _ = n
evalA (Var v)   s = get s v
evalA (a :+: b) s = evalA a s + evalA b s
evalA (a :*: b) s = evalA a s * evalA b s
evalA (a :-: b) s = evalA a s - evalA b s


a1 = undefined
a2 = undefined
as = undefined


------------------------- Part 3

data Bexp = Boolean Bool
          | Aexp :==: Aexp
          | Aexp :<=: Aexp
          | Neg Bexp
          | Bexp :&: Bexp
          | Bexp :|: Bexp
          | Bexp :&&: Bexp
          | Bexp :||: Bexp

evalB :: Bexp -> State -> Bool
evalB (Boolean b) _ = b
evalB (a :==: b)  s = evalA a s == evalA b s
evalB (a :<=: b)  s = evalA a s <= evalA b s
evalB (Neg b)     s = not (evalB b s)
evalB (a :&: b)   s = evalB a s && evalB b s
evalB (a :|: b)   s = evalB a s || evalB b s
evalB (a :&&: b)  s | evalB a s = evalB b s
                    | otherwise = False
evalB (a :||: b)  s | evalB a s = True
                    | otherwise = evalB b s


b1 = undefined
b2 = undefined
bs = undefined


------------------------- Part 4

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

c1 = undefined
c2 = undefined


------------------------- Part 5

-- data Chain =
