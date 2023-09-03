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
lkup :: String -> Memory -> Maybe Value
lkup x mem = lookup x mem

upd :: Memory -> (String, Value) -> Memory
upd mem (x, v) = (x, v) : filter (\(y, _) -> y /= x) mem

-- Funciones auxiliares
belongs :: Int -> [Int] -> Bool
belongs z c = z `elem` c

union :: [Int] -> [Int] -> [Int]
union c1 c2 = c1 ++ filter (\x -> not (x `elem` c1)) c2

intersection :: [Int] -> [Int] -> [Int]
intersection c1 c2 = filter (\x -> x `elem` c1) c2

difference :: [Int] -> [Int] -> [Int]
difference c1 c2 = filter (\x -> not (x `elem` c2)) c1

included :: [Int] -> [Int] -> Bool
included c1 c2 = all (\x -> x `elem` c2) c1

-- Evaluación de expresiones
eval :: Memory -> Exp -> (Memory, Value)
eval mem (Var x) = case lkup x mem of
    Just v -> (mem, v)
    Nothing -> error "Variable no encontrada en la memoria"
eval mem Empty = (mem, Set [])
eval mem (Singleton n) = (mem, Set [n])
eval mem (Member e1 e2) =
    let (mem', v1) = eval mem e1
        (mem'', v2) = eval mem' e2
    in case (v1, v2) of
        (Set c1, Set c2) -> (mem'', BoolValue (belongs (head c1) c2))
        _ -> error "Tipos incorrectos en Member"
eval mem (Union e1 e2) =
    let (mem', v1) = eval mem e1
        (mem'', v2) = eval mem' e2
    in case (v1, v2) of
        (Set c1, Set c2) -> (mem'', Set (union c1 c2))
        _ -> error "Tipos incorrectos en Union"
eval mem (Intersection e1 e2) =
    let (mem', v1) = eval mem e1
        (mem'', v2) = eval mem' e2
    in case (v1, v2) of
        (Set c1, Set c2) -> (mem'', Set (intersection c1 c2))
        _ -> error "Tipos incorrectos en Intersection"
eval mem (Difference e1 e2) =
    let (mem', v1) = eval mem e1
        (mem'', v2) = eval mem' e2
    in case (v1, v2) of
        (Set c1, Set c2) -> (mem'', Set (difference c1 c2))
        _ -> error "Tipos incorrectos en Difference"
eval mem (Subset e1 e2) =
    let (mem', v1) = eval mem e1
        (mem'', v2) = eval mem' e2
    in case (v1, v2) of
        (Set c1, Set c2) -> (mem'', BoolValue (included c1 c2))
        _ -> error "Tipos incorrectos en Subset"
eval mem (Assign x e) =
    let (mem', v) = eval mem e
    in (upd mem' (x, v), v)


-- -- Expresiones de conjuntos
-- conj1 :: Exp
-- conj1 = Union (Union (Singleton 1) (Singleton 2)) (Singleton 3)

conj1 :: Exp
conj1 = Singleton 1 `Union` Singleton 2 `Union` Singleton 3

-- conj2 :: Exp
-- conj2 = Singleton 2 `Union` Singleton 3 `Union` Singleton 4

-- conj3 :: Exp
-- conj3 = conj1 `Union` conj2

-- conj4 :: Exp
-- conj4 = conj1 `Intersection` conj2

-- pert1 :: Exp
-- pert1 = Member (Singleton 2) conj1

-- pert2 :: Exp
-- pert2 = Member (Singleton 3) conj4

-- incl1 :: Exp
-- incl1 = Subset conj1 conj2

-- incl2 :: Exp
-- incl2 = Subset conj4 (Singleton 2 `Union` Singleton 3 `Union` Singleton 4)

-- incl3 :: Exp
-- incl3 = Subset conj1 (conj1 `Union` conj2)

-- ass1 :: Exp
-- ass1 = Assign "w" conj1

-- ass2 :: Exp
-- ass2 = Assign "x" conj4

-- ass3 :: Exp
-- ass3 = Assign "y" pert2

-- ass4 :: Exp
-- ass4 = Assign "z" incl2

-- Ejemplo de uso
main :: IO ()
main = do
    let initialMem = []
    let (finalMem, result) = eval initialMem conj1
    putStrLn $ "Resultado: " ++ show result
