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
  Case (applySubstitution sigma er) (map (\(Branch c xs e) -> Branch c xs (applySubstitution (removeVars xs sigma) e)) branches)
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
  otherwise -> App e1 e2
reduce (Case er branches) = case reduce er of
  Const c -> case findBranch c branches of
    (Branch _ xs e) -> reduce (applySubstitution (zip xs (map Const (split c))) e)
  otherwise -> Case er (map (\(Branch c xs e) -> Branch c xs (reduce e)) branches)
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

-- ramaI: dado un  ́arbol binario, con informaci ́on en los nodos, y hojas
-- sin informaci ́on, retorna una lista con todos los elementos de la rama
-- izquierda
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

-- Ejemplo 1: Variable encontrada en la sustitución
-- applySubstitution sigma (Var "x") -- Devuelve: Const "5"

-- -- Ejemplo 2: Variable no encontrada en la sustitución
-- applySubstitution sigma (Var "z") -- Devuelve: Var "z"

-- -- Ejemplo 3: Aplicación de sustitución en una expresión más compleja
-- applySubstitution sigma (App (Var "x") (Var "y")) -- Devuelve: App (Const "5") (Var "z")

-- -- Ejemplo 4: Sustitución en una expresión lambda
-- applySubstitution sigma (Lambda "x" (Var "x")) -- Devuelve: Lambda "x" (Var "x")

-- -- Ejemplo 5: Sustitución en una expresión de caso (case)
-- let branch = Branch "True" [] (Var "x")
-- applySubstitution sigma (Case (Var "y") [branch]) -- Devuelve: Case (Var "y") [Branch "True" [] (Var "x")]

-- Ejemplos de expresiones de prueba
expression1 = App (Lambda "x" (Var "x")) (Const "A")
expression2 = App (Lambda "x" (App (Var "x") (Var "x"))) (Lambda "y" (Const "B"))


main :: IO ()
main = do
  let result1 = evaluateExpression expression1
  let result2 = evaluateExpression expression2
  putStrLn "Resultado de expression1:"
  print result1
  putStrLn "Resultado de expression2:"
  print result2
