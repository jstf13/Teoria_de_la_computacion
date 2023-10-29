data Program = Assignment String Expression
            | LocalDeclaration [String] Program
            | Sequence Program Program
            | Case String [Branch]
            | Iteration String [Branch]
            deriving (Show, Eq)

type Branch = (String, [String], Program)

data Expression = Constructor String [Expression]
                | Variable String
                deriving (Show, Eq)

data Value = ConstructorVal String [Value]
           | NullVal
              deriving (Show, Eq)

type Memory = [(String, Value)]

-- Función para buscar un valor en la memoria
busqueda :: String -> Memory -> Value
busqueda key [] = NullVal
busqueda key ((k, v):rest)
    | key == k = v
    | otherwise = busqueda key rest

-- Ejemplo para probar busqueda
-- busqueda "x" [("x", ConstructorVal "Z" [])]
-- busqueda "x" [("y", ConstructorVal "Z" [])]
-- busqueda "x" [("x", ConstructorVal "Z" []), ("y", ConstructorVal "Z" [])]

-- Función para actualizar la memoria con una lista de (clave, valor) a actualizar
actualizacion :: [(String, Value)] -> Memory -> Memory
actualizacion updates memory = actualizacionRec updates memory

-- Función recursiva para actualizar la memoria
actualizacionRec :: [(String, Value)] -> Memory -> Memory
actualizacionRec [] memory = memory  -- No hay más actualizaciones
actualizacionRec ((key, value):restUpdates) memory =
    let updatedMemory = buscarYReemplazar key value memory in
    actualizacionRec restUpdates updatedMemory

-- Función para buscar y reemplazar una clave en la memoria
buscarYReemplazar :: String -> Value -> Memory -> Memory
buscarYReemplazar key value [] = [(key, value)]
buscarYReemplazar key value ((k, v):rest) =
    case key == k of
        True -> (k, value) : rest
        False -> (k, v) : buscarYReemplazar key value rest

-- Ejemplo para probar actualizacion
-- actualizacion [("x", ConstructorVal "Z" [])] [("x", ConstructorVal "S" [ConstructorVal "Z" []])]
-- actualizacion [("x", ConstructorVal "Z" [])] [("y", ConstructorVal "S" [ConstructorVal "Z" []])]
-- actualizacion [("x", ConstructorVal "Z" [])] [("x", ConstructorVal "S" [ConstructorVal "Z" []]), ("y", ConstructorVal "S" [ConstructorVal "Z" []])]

-- Función para realizar la operación de alta en la memoria
alta :: [String] -> Memory -> Memory
alta [] memory = memory  -- No hay nuevas variables para agregar
alta (key:newKeys) memory =
    (key, NullVal) : alta newKeys memory

-- Ejemplo para probar alta
-- alta ["x"] [("y", ConstructorVal "S" [ConstructorVal "Z" []])]
-- alta ["x", "y"] [("y", ConstructorVal "S" [ConstructorVal "Z" []])]
-- alta ["x", "y", "z"] [("y", ConstructorVal "S" [ConstructorVal "Z" []])]


-- Función para realizar la operación de baja en la memoria
bajas :: [String] -> Memory -> Memory
bajas keysToDelete memory = eliminarVariables keysToDelete memory []

-- Función para eliminar las variables especificadas de la memoria
eliminarVariables :: [String] -> Memory -> Memory -> Memory
eliminarVariables [] memory newMemory = newMemory ++ memory  -- No hay más variables para eliminar
eliminarVariables (key:restKeys) [] newMemory = newMemory  -- No hay más variables en la memoria
eliminarVariables (key:restKeys) ((k, v):rest) newMemory =
    case key == k of
        True -> eliminarVariables restKeys rest newMemory
        False -> eliminarVariables (key:restKeys) rest ((k, v):newMemory)


-- Ejemplo para probar bajas
-- bajas ["x"] [("x", ConstructorVal "Z" [])]
-- bajas ["x", "y"] [("x", ConstructorVal "Z" []), ("y", ConstructorVal "S" [ConstructorVal "Z" []])]
-- bajas ["x", "y", "z"] [("x", ConstructorVal "Z" []), ("y", ConstructorVal "S" [ConstructorVal "Z" []]), ("z", ConstructorVal "S" [ConstructorVal "S" [ConstructorVal "Z" []]])]


-- Función para evaluar una expresión
evaluacionExpresion :: Expression -> Memory -> Value
evaluacionExpresion (Constructor c subexpresiones) memory =
    let valoresSubexpresiones = evaluacionListaExpresiones subexpresiones memory []
    in ConstructorVal c valoresSubexpresiones
evaluacionExpresion (Variable var) memory =
    case buscarVariable var memory of
        NullVal -> error ("Variable " ++ var ++ " no encontrada en la memoria")
        value -> value

-- Función para evaluar una lista de expresiones
evaluacionListaExpresiones :: [Expression] -> Memory -> [Value] -> [Value]
evaluacionListaExpresiones [] _ valoresSubexpresiones = valoresSubexpresiones
evaluacionListaExpresiones (exp:rest) memory valoresSubexpresiones =
    let valor = evaluacionExpresion exp memory
    in evaluacionListaExpresiones rest memory (valoresSubexpresiones ++ [valor])

-- Función para buscar una variable en la memoria
buscarVariable :: String -> Memory -> Value
buscarVariable _ [] = NullVal
buscarVariable var ((key, value):rest) =
    case var == key of
        True -> value
        False -> buscarVariable var rest

-- Ejemplo para probar evaluacionExpresion
-- evaluacionExpresion (Constructor "Z" []) [("x", ConstructorVal "Z" [])]
-- evaluacionExpresion (Constructor "S" [Constructor "Z" []]) [("x", ConstructorVal "Z" [])]
-- evaluacionExpresion (Variable "x") [("x", ConstructorVal "Z" [])]
-- evaluacionExpresion (Variable "x") [("y", ConstructorVal "Z" [])]
-- evaluacionExpresion (Variable "x") [("x", ConstructorVal "Z" []), ("y", ConstructorVal "S" [ConstructorVal "Z" []])]


-- Función para realizar asignaciones en la memoria
asignacion :: [String] -> [Expression] -> Memory -> Memory
asignacion [] [] memory = memory  -- No hay más asignaciones
asignacion (var:vars) (exp:exprs) memory =
    let updatedMemory = asignarVariable var exp memory
    in asignacion vars exprs updatedMemory

-- Función para asignar una variable con su respectiva expresión en la memoria
asignarVariable :: String -> Expression -> Memory -> Memory
asignarVariable var exp memory =
    let valorExpresion = evaluacionExpresion exp memory
    in actualizarVariable var valorExpresion memory

-- Función para actualizar el valor de una variable en la memoria
actualizarVariable :: String -> Value -> Memory -> Memory
actualizarVariable var value [] = [(var, value)]  -- Agregar la variable si no existe
actualizarVariable var value memory =
    let updatedMemory = buscarYReemplazar var value memory
    in case updatedMemory == memory of
        True -> (var, value) : memory  -- Agregar la variable si no existe
        False -> updatedMemory

-- Ejemplo para probar asignacion
-- asignacion ["x"] [Constructor "Z" []] [("x", ConstructorVal "S" [ConstructorVal "Z" []])]
-- asignacion ["x", "y"] [Constructor "Z" [], Constructor "S" [Constructor "Z" []]] [("x", ConstructorVal "S" [ConstructorVal "Z" []])]
-- asignacion ["x", "y", "z"] [Constructor "Z" [], Constructor "S" [Constructor "Z" []], Constructor "S" [Constructor "S" [Constructor "Z" []]]] [("x", ConstructorVal "S" [ConstructorVal "Z" []])]
-- asignacion ["x", "y", "z"] [Constructor "Z" [], Constructor "S" [Constructor "Z" []], Constructor "S" [Constructor "S" [Constructor "Z" []]]] [("x", ConstructorVal "S" [ConstructorVal "Z" []]), ("y", ConstructorVal "S" [ConstructorVal "Z" []])]

-- Función para realizar declaraciones locales en la memoria
local :: [String] -> Program -> Memory -> Memory
local [] _ memory = memory  -- No hay más declaraciones locales
local (var:vars) program memory =
    let updatedMemory = ejecutarPrograma program memory
    in local vars program updatedMemory

-- Función para ejecutar un programa Imp con una memoria inicial
ejecutarPrograma :: Program -> Memory -> Memory
ejecutarPrograma (Assignment var exp) memory = asignacion [var] [exp] memory
ejecutarPrograma (LocalDeclaration vars program) memory = local vars program memory
ejecutarPrograma (Sequence p1 p2) memory = ejecutarPrograma p2 (ejecutarPrograma p1 memory)
ejecutarPrograma (Case var branches) memory = ejecutarSeleccion var branches memory
ejecutarPrograma (Iteration var branches) memory = ejecutarIteracion var branches memory

-- Función para ejecutar una selección
ejecutarSeleccion :: String -> [Branch] -> Memory -> Memory
ejecutarSeleccion var [] memory = memory  -- No hay más branches
ejecutarSeleccion var ((constructor, vars, program):rest) memory =
    let valorVar = buscarVariable var memory
    in case valorVar of
        NullVal -> error ("Variable " ++ var ++ " no encontrada en la memoria")
        ConstructorVal c subvalores ->
            case c == constructor of
                True -> ejecutarPrograma program (actualizacion (zip vars subvalores) memory)
                False -> ejecutarSeleccion var rest memory

-- Función para ejecutar una iteración
ejecutarIteracion :: String -> [Branch] -> Memory -> Memory
ejecutarIteracion var branches memory =
    let valorVar = buscarVariable var memory
    in case valorVar of
        NullVal -> error ("Variable " ++ var ++ " no encontrada en la memoria")
        ConstructorVal c subvalores ->
            case branches of
                [] -> memory  -- No hay más branches
                ((constructor, vars, program):rest) ->
                    case c == constructor of
                        True -> ejecutarPrograma program (actualizacion (zip vars subvalores) memory)
                        False -> ejecutarIteracion var rest memory


-- Función para ejecutar una secuencia de programas
sec :: [Program] -> Memory -> Memory
sec [] memory = memory  -- No hay más programas que ejecutar
sec (p:ps) memory = sec ps (ejecutarPrograma p memory)

-- Ejemplo para probar sec
-- sec [Assignment "x" (Constructor "Z" []), Assignment "y" (Constructor "S" [Constructor "Z" []])] [("y", ConstructorVal "S" [ConstructorVal "Z" []])]
-- sec [Assignment "x" (Constructor "Z" []), Assignment "y" (Constructor "S" [Constructor "Z" []]), Assignment "z" (Constructor "S" [Constructor "S" [Constructor "Z" []]])] [("y", ConstructorVal "S" [ConstructorVal "Z" []])]

dos = ConstructorVal "S" [ConstructorVal "S" [ConstructorVal "Z" []]]
tres = ConstructorVal "S" [ConstructorVal "S" [ConstructorVal "S" [ConstructorVal "Z" []]]]

-- Función 'par' para determinar si un número natural es par
par :: Value -> Bool
par (ConstructorVal "Z" []) = True
par (ConstructorVal "S" [n]) = not (par n)
par _ = False

-- Función 'suma' para calcular la suma de dos números naturales
suma :: Value -> Value -> Value
suma (ConstructorVal "Z" []) n = n
suma (ConstructorVal "S" [m]) n = ConstructorVal "S" [suma m n]
suma _ _ = NullVal

-- ejemplo para probar suma
-- suma dos tres

-- Función 'largo' para calcular la cantidad de elementos en una lista
largo :: Value -> Int
largo (ConstructorVal "NullVal" []) = 0
largo (ConstructorVal "Cons" [_, xs]) = 1 + largo xs
largo _ = -1  -- Valor no válido (en caso de que no sea una lista)

-- ejemplo para probar largo
-- largo (ConstructorVal "Cons" [ConstructorVal "Z" [], ConstructorVal "Nil" []])
-- largo (ConstructorVal "Cons" [ConstructorVal "Z" [], ConstructorVal "Cons" [ConstructorVal "Z" [], ConstructorVal "Nil" []]])

-- Función de igualdad de valores
equalN :: Value -> Value -> Bool
equalN (ConstructorVal "Z" []) (ConstructorVal "Z" []) = True
equalN (ConstructorVal "S" [m]) (ConstructorVal "S" [n]) = equalN m n
equalN _ _ = False

-- Ejemplos de prueba de la funcion equalN
-- equalN (ConstructorVal "Z" []) (ConstructorVal "Z" [])
-- equalN (ConstructorVal "S" [ConstructorVal "Z" []]) (ConstructorVal "S" [ConstructorVal "Z" []])

-- Función 'concatenate' para concatenar dos listas
concatenate :: Value -> Value -> Value
concatenate (ConstructorVal "Nil" []) list = list
concatenate (ConstructorVal "Cons" [x, xs]) list = ConstructorVal "Cons" [x, concatenate xs list]
concatenate _ _ = NullVal


-- Ejemplos de prueba de la funcion concat
-- concatenate (ConstructorVal "Cons" [ConstructorVal "Z" [], ConstructorVal "Nil" []]) (ConstructorVal "Cons" [ConstructorVal "Z" [], ConstructorVal "Nil" []])
-- concatenate (ConstructorVal "Cons" [ConstructorVal "Z" [], ConstructorVal "Nil" []]) (ConstructorVal "Cons" [ConstructorVal "S" [ConstructorVal "Z" []], ConstructorVal "Nil" []])

-- Rsultado esperado de concatenate
-- ConstructorVal "Cons" [ConstructorVal "Z" [], ConstructorVal "Cons" [ConstructorVal "Z" [], ConstructorVal "Cons" [ConstructorVal "S" [ConstructorVal "Z" []], ConstructorVal "Nil" []]]]
