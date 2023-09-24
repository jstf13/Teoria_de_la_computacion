data Exp
  = Var String
  | Const String
  | Lambda String Exp
  | App Exp Exp
  | Case Exp [Branch]
  | Rec String Exp
  deriving (Show, Eq)

data Branch 
  = Branch String String Exp 
  deriving (Show, Eq)

type Substitution = [(String, Exp)]

-- lookupVar :: String -> Substitution -> Exp
-- lookupVar x sigma = fromMaybe (Var x) (lookup x sigma)

-- deleteVars :: [String] -> Substitution -> Substitution
-- deleteVars [] sigma = sigma
-- deleteVars (x:xs) sigma = deleteVars xs (filter (\(y, _) -> y /= x) sigma)

reduce :: Exp -> Maybe Exp
reduce (App (Lambda x e1) e2) = Just (subst x e1 e2)
reduce (Case (Const c) branches) = lookupBranch c branches
reduce _ = Nothing

subst :: String -> Exp -> Exp -> Exp
subst x e (Var y) | x == y = e
subst x e (Lambda y body) = Lambda y (subst x e body)
subst x e (App e1 e2) = App (subst x e e1) (subst x e e2)
subst x e (Case exp y:ys) = Case (subst x e exp) (myMap (substBranch x e) (y:ys))
subst x e (Rec y body) | x == y = Rec y body
subst x e (Rec y body) = Rec y (subst x e body)

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

substBranch :: String -> Exp -> Branch -> Branch
substBranch x e (Branch c y body) = Branch c y (subst x e body)

lookupBranch :: String -> [Branch] -> Maybe Exp
lookupBranch _ [] = Nothing
lookupBranch c (Branch c' x body : branches)
  | c == c' = Just (subst x (Const c) body)
  | otherwise = lookupBranch c branches

eval :: Exp -> Exp
eval exp = case reduce exp of
  Just exp' -> eval exp'
  Nothing -> exp

and :: Exp
and = Lambda "x" (Lambda "y" (Case (Var "x") [Branch "True" "x" (Var "y"), Branch "False" "x" (Const "False")]))

duplicar :: Exp
duplicar = Lambda "n" (Case (Var "n") [Branch "O" "_" (Const "O"), Branch "S" "n" (App (Const "S") (App (Const "S") (Var "n")))])

unir :: Exp
unir = Lambda "l1" (Lambda "l2" (Case (Var "l1") [Branch "[]" "_" (Var "l2"), Branch ":" "x" (App (Const ":") (App (App unir (Var "xs")) (Var "l2")))]))

ramaI :: Exp
ramaI = Lambda "tree" (Case (Var "tree") [Branch "Leaf" "_" (Const "[]"), Branch "Node" "x" (App (Var "x") (App ramaI (Var "xs")))])

-- Define la función "and" en χ embebido en Haskell
andFunction :: Exp
andFunction = and

-- Define la función "duplicar" en χ embebido en Haskell
duplicarFunction :: Exp
duplicarFunction = duplicar

-- Define la función "unir" en χ embebido en Haskell
unirFunction :: Exp
unirFunction = unir

-- Define la función "ramaI" en χ embebido en Haskell
ramaIFunction :: Exp
ramaIFunction = ramaI

-- Prueba la función "unir" con una lista que contiene al 0 y al 1, y otra que tiene al 2 y al 3.
pruebaUnir :: Exp
pruebaUnir = App (App unirFunction (Const "0")) (Const "1")

-- Prueba la función "ramaI" con un árbol binario de al menos 3 niveles en la rama izquierda.
pruebaRamaI :: Exp
pruebaRamaI = App ramaIFunction tree
  where 
    tree =
      App
        (App
          (Const "Node")
          (Lambda "x" (App (Var "x") (Var "xs"))))
        (App
          (App
            (Const "Node")
            (Lambda "x" (App (Var "x") (Var "xs"))))
          (App
            (App
              (Const "Node")
              (Lambda "x" (App (Var "x") (Var "xs"))))
            (Const "Leaf")))

main :: IO ()
main = do
  putStrLn "Resultado de la función 'unir' con [0, 1] y [2, 3]:"
  print (eval pruebaUnir)

  putStrLn "\nResultado de la función 'ramaI' con árbol de prueba:"
  print (eval pruebaRamaI)
