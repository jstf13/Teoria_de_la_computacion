-- Definición de tipos
data Exp = Var String
         | Empty
         | Singleton Int
         | Member Exp Exp
         | Union Exp Exp
         | Intersection Exp Exp
         | Difference Exp Exp
         | Subset Exp Exp
         | Assign String Exp
         deriving (Show, Eq)

data Value = Set [Int]
           | BoolValue Bool
           deriving (Show, Eq)

type Memory = [(String, Value)]

-- Operaciones sobre la memoria
lkup :: String -> Memory -> Value
lkup _ [] = error "Variable no encontrada en la memoria"
lkup s ((x, y):mem)
    | s == x = y
    | otherwise = lkup s mem

upd :: Memory -> (String, Value) -> Memory
upd [] (a, b) = error "Variable no encontrada en la memoria"
upd ((x, y):mem) (a, b) 
    | x == a = ((a, b):mem)
    | otherwise = (x, y) : upd mem (a, b)

-- Funciones auxiliares
myNot :: Bool -> Bool
myNot True = False
myNot False = True

belongs :: Int -> [Int] -> Bool
belongs _ [] = False
belongs z (x:xs)
    | x == z = True
    | otherwise = belongs z xs

union :: [Int] -> [Int] -> [Int]
union [] ls = ls
union xs [] = xs
union (x:xs) ls
    | belongs x ls == True = union xs ls
    | belongs x ls == False = union xs (x:ls)

intersection :: [Int] -> [Int] -> [Int]
intersection [] ls = []
intersection xs [] = xs
intersection (x:xs) ls
    | belongs x ls == True = x : intersection xs ls
    | belongs x ls == False = intersection xs ls

difference :: [Int] -> [Int] -> [Int]
difference [] _ = []
difference xs [] = xs
difference (x:xs) ys
    | belongs x ys = difference xs ys
    | otherwise = x : difference xs ys

included :: [Int] -> [Int] -> Bool
included [] _ = True
included xs [] = False
included (x:xs) ls
    | belongs x ls == True = included xs ls
    | belongs x ls == False = False

eval :: Memory -> Exp -> (Memory, Value)
eval mem Empty = (mem, Set [])
eval mem (Var x) = case lkup x mem of
    Set c -> (mem, Set c)
    BoolValue b -> (mem, BoolValue b)
eval mem (Singleton x) = (mem, Set [x])
eval mem (Member e1 e2) = case (eval mem e1, eval mem e2) of
    ((_, Set c1), (_, Set c2)) -> 
        (mem, BoolValue (any (\x -> belongs x c1) c2))
    _ -> error "Tipos incorrectos en Member"
eval mem (Union e1 e2) = case (eval mem e1, eval mem e2) of
    ((_, Set c1), (_, Set c2)) -> (mem, Set (union c1 c2))
eval mem (Intersection e1 e2) = case (eval mem e1, eval mem e2) of
    ((_, Set c1), (_, Set c2)) -> (mem, Set (intersection c1 c2))
eval mem (Difference e1 e2) = case (eval mem e1, eval mem e2) of
    ((_, Set c1), (_, Set c2)) -> (mem, Set (difference c1 c2))
eval mem (Subset e1 e2) = case (eval mem e1, eval mem e2) of
    ((_, Set c1), (_, Set c2)) -> (mem, BoolValue (included c1 c2))
eval mem (Assign x e1) = case (eval mem e1) of
    (m1, Set c1) -> (upd m1 (x, Set c1), Set c1)
    

conj1 :: Exp
conj1 = Singleton 1 `Union` Singleton 2 `Union` Singleton 3

conj2 :: Exp
conj2 = Union (Singleton 2) (Union (Singleton 3) (Singleton 4))

conj3 :: Exp
conj3 = conj1 `Union` conj2

conj4 :: Exp
conj4 = Intersection conj1 conj2

pert1 :: Exp
pert1 = Member (Singleton 2) conj1

pert2 :: Exp
pert2 = Member (Singleton 3) conj4

incl1 :: Exp
incl1 = Subset conj1 conj2

incl2 :: Exp
incl2 = Subset conj4 conj2

incl3 :: Exp
incl3 = Subset conj1 (conj1 `Union` conj2)

ass1 :: Exp
ass1 = Assign "w" conj1

ass2 :: Exp
ass2 = Assign "x" conj4

ass3 :: Exp
ass3 = Assign "y" pert2

ass4 :: Exp
ass4 = Assign "z" incl2




-- Ejemplo de una memoria con variables definidas
sampleMemory :: Memory
sampleMemory = [
    ("x", Set [1, 2, 3]),          -- Asignación de un conjunto
    ("y", Set [2, 3, 4]),          -- Asignación de otro conjunto
    ("z", BoolValue True)          -- Asignación de un valor booleano
    ]

-- Ejemplo de definicion de variables
var1 :: Exp
var1 = Var "x"

var2 :: Exp
var2 = Var "y"

var3 :: Exp
var3 = Var "j"