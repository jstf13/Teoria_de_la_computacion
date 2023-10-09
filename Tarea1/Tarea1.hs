-- Definición de tipos de datos según la sintaxis abstracta
data Exp
  = Var String
  | Const String
  | Lambda String Exp
  | App Exp Exp
  | Case Exp [Branch]
  | Rec String Exp
  deriving (Show, Eq)

data Branch
  = Branch String [String] Exp
  deriving (Show, Eq)

-- Tipo para representar sustituciones
type Substitution = [(String, Exp)]

-- Aplicar una sustitución a una expresión
applySubstitution :: Substitution -> Exp -> Exp
applySubstitution sigma (Var x) = lkup x sigma 
applySubstitution sigma (Lambda x e) = Lambda x (applySubstitution (removeVars [x] sigma) e)
applySubstitution sigma (App e1 e2) = App (applySubstitution sigma e1) (applySubstitution sigma e2)
applySubstitution sigma (Case er branches) =
  Case (applySubstitution sigma er) (myMap (\(Branch c xs e) -> Branch c xs (applySubstitution (removeVars xs sigma) e)) branches)
applySubstitution sigma (Rec x e) = Rec x (applySubstitution (removeVars [x] sigma) e)
applySubstitution _ e = e

-- Función auxiliar para buscar en una sustitución
lkup :: String -> Substitution -> Exp
lkup x [] = Var x
lkup x ((y, e) : sigma)
  | x == y = e
  | otherwise = lkup x sigma

-- Función auxiliar para eliminar variables de una sustitución
removeVars :: [String] -> Substitution -> Substitution
removeVars [] sigma = sigma
removeVars (x : xs) sigma = case sigma of
  ((y, e): rest) | x == y -> removeVars xs rest
                 | otherwise -> (y, e) : removeVars xs rest
  [] -> []

-- Función de reducción (parcial)
reduce :: Exp -> Exp
reduce (Var x) = Var x
reduce (Const c) = Const c
reduce (Lambda x e) = Lambda x (reduce e)
reduce (App e1 e2) = case reduce e1 of
  Lambda x e -> reduce (applySubstitution [(x, e2)] e)
  Rec f e -> reduce (applySubstitution [(f, Rec f e)] e)  -- Agregado para manejar funciones definidas con Rec
  otherwise -> App e1 e2
reduce (Case er branches) = case reduce er of
  Const c -> case findBranch c branches of
    (Branch _ xs e) -> reduce (applySubstitution (zip xs (myMap Const (split c))) e)
  otherwise -> Case er (myMap (\(Branch c xs e) -> Branch c xs (reduce e)) branches)
reduce (Rec x e) = reduce (applySubstitution [(x, Rec x e)] e)


-- Función auxiliar para encontrar una rama en un case
findBranch :: String -> [Branch] -> Branch
findBranch _ [] = error "No se encontró la rama"
findBranch c ((Branch c' xs e) : branches)
  | c == c' = (Branch c' xs e)
  | otherwise = findBranch c branches

-- Función auxiliar para separar una cadena
split :: String -> [String]
split [] = []
split (x:xs) = [x] : split xs

-- Función auxiliar para mapear una función sobre una lista
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs


-- -- Función de evaluación de expresiones
evaluateExpression :: Exp -> Exp
evaluateExpression e = case reduce e of
  e' | e' == e -> e
     | otherwise -> evaluateExpression e'


-- Funciones embebidas en Haskell (χ embebido)
myAnd :: Exp
myAnd = Lambda "x" (Lambda "y" (Case (Var "x") [Branch "True" [] (Var "y"), Branch "False" [] (Const "False")]))

duplicar :: Exp
duplicar = App (Rec "duplicar" (Lambda "x" (Case (Var "x") [Branch "0" [] (Const "0"), Branch "n" ["n"] (App (App (Const "+") (Const "1")) (App (App (Var "duplicar") (Var "n")) (Var "n")))]))) (Var "x")

unir :: Exp
unir = App (Rec "unir" (Lambda "x" (Lambda "y" (Case (Var "x") [Branch "[]" [] (Var "y"), Branch "x" ["x"] (App (App (Const ":") (Var "x")) (App (App (Var "unir") (Var "x")) (Var "y")))])))) (Var "x")

ramaI :: Exp
ramaI = App (Rec "ramaI" (Lambda "x" (Case (Var "x") [Branch "[]" [] (Const "[]"), Branch "x" ["x"] (App (App (Const ":") (App (App (Const "head") (Var "x")) (Const "[]"))) (App (App (Var "ramaI") (App (Const "tail") (Var "x"))) (Const "[]")))]))) (Var "x")





-- Definiciones de variables de ejemplo
varX = Var "x"
varY = Var "y"
varZ = Var "z"

listVar1 = ["x"]
listVar2 = ["x", "y"]

constA = Const "A"
constB = Const "B"

lambdaExp = Lambda "x" (App varX varX)

branch1 = Branch "Branch1" ["x", "y"] (App varX varY)
branch2 = Branch "Branch2" ["z"] (App varZ constA)
branch3 = Branch "Branch3" [] constB

substitution1 = [("x", constA), ("y", constB)]
substitution2 = [("z", constB)]

-- Ejemplo de expresión para reducir
expressionToReduce = App (Lambda "x" (App varX varX)) constA

-- Definición de una sustitución
sigma = [("x", Const "5"), ("y", Var "z")]

-- Ejemplos de expresiones de prueba
expression1 = App (Lambda "x" (Var "x")) (Const "A")
expression2 = App (Lambda "x" (App (Var "x") (Var "x"))) (Lambda "y" (Const "B"))

-- Arbol binario
arbolBinario :: Exp
arbolBinario =
  App
    (App (Const "Node") (Const "1")) -- Nodo raíz
    (App
      (App (Const "Node") (Const "2")) -- Subárbol izquierdo
      (App (App (Const "Node") (Const "3")) (Const "4")) -- Subárbol derecho
    )