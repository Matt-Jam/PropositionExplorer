import Data.Char (isAlpha)
data PropConstructors = UnaryFunction (Prop -> Prop) | BinaryFunction (Prop -> Prop -> Prop)
type Stack a = [a]
data Prop = Const Bool
            | Var Char
            | Not Prop
            | And Prop Prop
            | Imply Prop Prop
            | Or Prop Prop
            | Equiv Prop Prop
            deriving (Eq, Show)

type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool

operatorMapping :: Assoc Char PropConstructors
operatorMapping = [('+',BinaryFunction Or),('*', BinaryFunction And),('-',UnaryFunction Not)]

operators :: Assoc Char Int
operators = [('*',2),('+',1),('-',3)]

push :: Stack a -> a -> Stack a
push s a = a : s

pop :: Stack a -> a
pop = head

isEmpty :: Stack a -> Bool
isEmpty = null

find :: Eq k => k -> Assoc k v -> v
find k a = head [v | (k',v) <- a, k == k']

p1 :: Prop
p1 = And (Const True) (Or (Const False) (Not (Not (Var 'c'))))

reduce :: Prop -> Prop 
reduce p 
    | isTaut p = Const True 
    | isContradiction p = Const False
    | otherwise = simplify p

simplify :: Prop -> Prop
simplify (Const b) = Const b
simplify (Var x) =  Var x
simplify (Not (Not p)) = simplify p
simplify (Not (Const True)) = Const False
simplify (Not (Const False)) = Const True
simplify (And (Const True) p) = simplify p
simplify (And p (Const True)) =  simplify p
simplify (Or p (Const False)) = simplify p
simplify (Or (Const False) p) = simplify p
simplify (And (Const False) p) = Const False
simplify (And p (Const False)) =  Const False
simplify (Or p (Const True)) = Const True
simplify (Or (Const True) p) = Const True
simplify (Not p) = Not (simplify p)
simplify (And p p') = And (simplify p) (simplify p')
simplify (Or p p') = Or (simplify p) (simplify p')
simplify (Imply p p') = Imply (simplify p) (simplify p')
simplify (Equiv p p') = Equiv (simplify p) (simplify p')

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

isContradiction :: Prop -> Bool 
isContradiction p = not (or [eval s p | s <- substs p])

truthTable :: Prop -> [([Bool],Bool)]
truthTable p = [(i,eval (zip l i) p)|i<-bools (length l)]
                where l = rmdups (vars p)

isOperator :: Char -> Bool
isOperator c = any (\(k,v) -> k == c) operators

handleOperator :: Char -> Stack Char -> Stack Char -> (Stack Char, Stack Char)
handleOperator c [] out = ([c],out)
handleOperator c (op:ops) out
    | op == '(' = (c:op:ops,out)
    | find c operators <= find op operators = handleOperator c ops (op:out)
    | otherwise = (c:op:ops,out)

handleRightBracket :: Stack Char -> Stack Char -> (Stack Char, Stack Char)
handleRightBracket ('(':ops) out = (ops,out)
handleRightBracket (op:ops) out = handleRightBracket ops (op:out)

handleChar:: Char -> Stack Char -> Stack Char -> (Stack Char, Stack Char)
handleChar c op out
    | c == '(' = (c:op,out)
    | isOperator c = handleOperator c op out
    | c == ')' = handleRightBracket op out
    | isAlpha c = (op,c:out)
    | otherwise = (op,out)


parseExpression :: String -> Stack Char -> Stack Char -> Stack Char
parseExpression [] [] out = out
parseExpression [] (op:ops) out = parseExpression [] ops (op : out)
parseExpression (c:s) op out = uncurry (parseExpression s) (handleChar c op out)

constructProp :: Char -> Stack Prop -> Stack Prop
constructProp c ps = case find c operatorMapping of
    UnaryFunction f ->  reduce (f (head ps)) : tail ps
    BinaryFunction f ->  reduce (f (head (tail ps)) (head ps)) : tail (tail ps)

convertParsedExpression :: Stack Char -> Stack Prop -> Prop
convertParsedExpression [] ps = head ps
convertParsedExpression (c:cs) ps
    | isOperator c = convertParsedExpression cs (constructProp c ps)
    | c == 'T' = convertParsedExpression cs (Const True : ps)
    | c == 'F' = convertParsedExpression cs (Const False : ps)
    | otherwise = convertParsedExpression cs (Var c : ps)