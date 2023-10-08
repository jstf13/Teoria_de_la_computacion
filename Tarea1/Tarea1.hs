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
applySubstitution sigma (Var x) = case lkup x sigma of
  Const c -> Const c
  e -> e
applySubstitution sigma (Lambda x e) = Lambda x (applySubstitution (filter (\(y, _) -> y /= x) sigma) e)
applySubstitution sigma (App e1 e2) = App (applySubstitution sigma e1) (applySubstitution sigma e2)
applySubstitution sigma (Case er branches) =
  Case (applySubstitution sigma er) (map (\(Branch c xs e) -> Branch c xs (applySubstitution (removeVars xs sigma) e)) branches)
applySubstitution sigma (Rec x e) = Rec x (applySubstitution (filter (\(y, _) -> y /= x) sigma) e)
applySubstitution _ e = e

-- Función auxiliar para buscar en una sustitución
lkup :: String -> Substitution -> Exp
lkup _ [] = error "Variable no encontrada en la sustitución"
lkup x ((y, e) : sigma)
  | x == y = e
  | otherwise = lkup x sigma

-- Función auxiliar para eliminar variables de una sustitución
removeVars :: [String] -> Substitution -> Substitution
removeVars [] sigma = sigma
removeVars (x : xs) sigma = removeVars xs (filter (\(y, _) -> y /= x) sigma)

-- Función de reducción (parcial)
reduce :: Exp -> Exp
reduce (App (Lambda x e1) e2) = applySubstitution [(x, e2)] e1
-- Define las reglas de reducción aquí según la especificación

-- -- Función de evaluación de expresiones
-- evaluateExpression :: Exp -> Exp
-- evaluateExpression exp =
--   case reduce exp of
--     exp' | exp' == exp -> exp' -- No se puede reducir más
--     _ -> evaluateExpression exp'

-- -- Funciones embebidas en Haskell (χ embebido)
-- and :: Exp
-- and =
--   Lambda "x" $
--     Lambda "y" $
--       Case (Var "x")
--         [ Branch "True" [] (Var "y"),
--           Branch "False" [] (Const "False")
--         ]

-- duplicar :: Exp
-- duplicar =
--   Lambda "n" $
--     App
--       (App (Rec "+" (Var "x")) (Var "n"))
--       (Var "n")

-- unir :: Exp
-- unir =
--   Lambda "l1" $
--     Lambda "l2" $
--       Case (Var "l1")
--         [ Branch "[]" [] (Var "l2"),
--           Branch ":" ["x", "xs"] (Const ":")
--         ]

-- -- Pruebas
-- main :: IO ()
-- main = do
--   let test1 = App (App and (Const "True")) (Const "False")
--   let test2 = App (App duplicar (Const "3")) (Const "3")
--   let test3 = App (App unir (Const "[1, 2]")) (Const "[3, 4]")
--   let test4 = App (App unir (Const "[1, 2]")) (Const "False")

--   putStrLn "Test 1 (and True False):"
--   print (evaluateExpression test1)

--   putStrLn "Test 2 (duplicar 3 3):"
--   print (evaluateExpression test2)

--   putStrLn "Test 3 (unir [1, 2] [3, 4]):"
--   print (evaluateExpression test3)

--   putStrLn "Test 4 (unir [1, 2] False):"
--   print (evaluateExpression test4)


-- Definiciones de variables de ejemplo
varX = Var "x"
varY = Var "y"
varZ = Var "z"

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

