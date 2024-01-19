type Stack a = [a]
data Prop = Const Bool
            | Var Char
            | Not Prop
            | And Prop Prop
            | Imply Prop Prop
            | Or Prop Prop
            | Equiv Prop Prop

type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool

push :: Stack a -> a -> Stack a 
push s a = a : s 

pop :: Stack a -> a 
pop s = head s

isEmpty :: Stack a -> Bool 
isEmpty s = null s


find :: Eq k => k -> Assoc k v -> v
find k a = head [v | (k',v) <- a, k == k']

p1 :: Prop
p1 = Or (Var 'a') (Var 'c')

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p p') = eval s p && eval s p'
eval s (Imply p p') = eval s p <= eval s p'
eval s (Or p p') = eval s p || eval s p'
eval s (Equiv p p') = eval s p == eval s p'

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p p') = vars p ++ vars p'
vars (Imply p p') = vars p ++ vars p'
vars (Or p p') = vars p ++ vars p'
vars (Equiv p p') = vars p ++ vars p'

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (True :) bss ++ map (False :) bss
            where bss = bools (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/=x) (rmdups xs)

substs :: Prop -> [Subst]
substs p = map (zip l) (bools (length l))
            where l = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

truthTable :: Prop -> [([Bool],Bool)]
truthTable p = [(i,eval (zip l i) p)|i<-bools(length l)]
                where l = rmdups (vars p)

ParseExpression :: String -> Stack Char
