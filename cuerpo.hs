-- DATOS Y SHOW

data Modificacion = Insertar Integer Char | Borrar Integer | Substituir Integer Char deriving (Show, Eq)

type PaqueteModificaciones = [Modificacion]

data Archivo = ArchivoVacio | NuevaVersion PaqueteModificaciones Archivo
instance Show Archivo where
    show ArchivoVacio = "Archivo vacio"
    show file = "Archivo: " ++ obtenerUltimaVersion file

data SCV = NuevoSCV | AgregarArchivo Archivo SCV
instance Show SCV where
    show NuevoSCV = "SCV vacio"
    show scv = verArchivos scv

verArchivos :: SCV -> String
verArchivos NuevoSCV = ""
verArchivos (AgregarArchivo file scv) = "- " ++ (show file) ++ "\n" ++ (verArchivos scv)

-- EJERCICIOS

-- Ejercicio 1/8
aplicarModificacion :: String -> Modificacion -> String
aplicarModificacion s (Insertar idx v) 	 = first s idx ++ [v] ++ second s idx
aplicarModificacion s (Borrar idx) 	     = init (first s idx) ++ second s idx
aplicarModificacion s (Substituir idx v) = 
	aplicarModificacion (aplicarModificacion s (Borrar idx)) (Insertar (idx - 1) v)

first :: String -> Integer -> String
first lst idx = take (fromIntegral idx) lst

second :: String -> Integer -> String
second lst idx = drop (fromIntegral idx) lst

-- Ejemplos:
-- Main> aplicarModificacion "d" (Insertar 1 'a')
-- "da"
-- Main> aplicarModificacion "d" (Insertar 0 'a')
-- "ad"
-- Main> aplicarModificacion "dato" (Borrar 1)
-- "ato"

-- Ejercicio 2/8
aplicarPaqueteModificaciones :: String -> PaqueteModificaciones -> String
aplicarPaqueteModificaciones s []     =  s
aplicarPaqueteModificaciones s (x:xs) =  aplicarPaqueteModificaciones (aplicarModificacion s x) xs

-- Ejemplos:
-- Main> aplicarPaqueteModificaciones "dato" [Substituir 1 'p', Insertar 4 's']
-- "patos"

-- Ejercicio 3/8
obtenerUltimaVersion :: Archivo -> String
obtenerUltimaVersion ArchivoVacio                   = "Archivo vacio"
obtenerUltimaVersion (NuevaVersion pm ArchivoVacio) = aplicarPaqueteModificaciones "" pm
obtenerUltimaVersion (NuevaVersion pm nv)           = aplicarPaqueteModificaciones (obtenerUltimaVersion nv) pm


-- Ejemplos: (ver def. archivo1 y archivo2 abajo)
-- Main> obtenerUltimaVersion archivo1
-- "dato"
-- Main> obtenerUltimaVersion archivo2
-- "ddato"

-- Ejercicio 4/8
cantVersiones :: Archivo -> Integer
cantVersiones ArchivoVacio        = 0
cantVersiones (NuevaVersion _ nv) = 1 + cantVersiones nv

-- Ejemplos:
-- Main> cantVersiones archivo1
-- 1
-- Main> cantVersiones archivo2
-- 2

-- Ejercicio 5/8
obtenerVersion :: Integer -> Archivo -> String
obtenerVersion _ ArchivoVacio         = "Archivo vacio"
obtenerVersion 0 (NuevaVersion pm _)  = aplicarPaqueteModificaciones "" pm
obtenerVersion v (NuevaVersion pm nv) = obtenerVersion (v - 1) nv

-- Ejemplos:
-- Main> obtenerVersion 1 archivo2
-- "dato"

-- Ejercicio 6/8
levenshtein :: String -> String -> Integer --PaqueteModificaciones
levenshtein s [] = len s
levenshtein [] t = len t
levenshtein s t  = minimum [a, b, c]
	where
		a = (levenshtein (init s) t) +1
		b = (levenshtein s (init t)) +1
		c = (levenshtein (init s) (init t)) +
			if last s == last t then 0 else 1

-- Ejemplos:
-- Main> levenshtein "auto" "automata"
-- 4

-- Ejercicio 7/8
levenshtein2 :: String -> String -> PaqueteModificaciones
levenshtein2 s [] = insertarTodos s
levenshtein2 [] t = insertarTodos t
levenshtein2 s t  = minLength [a, b, c] 
	where 
		a = Borrar (len s) : levenshtein2 (init s) t
		b = Insertar (len s) (last t) : levenshtein2 s (init t)
		c = if last s == last t 
			then
				levenshtein2 (init s) (init t) 
			else
				Substituir (len s) (last t) : levenshtein2 (init s) (init t)

insertarTodos :: String -> PaqueteModificaciones
insertarTodos []     = []
insertarTodos (x:xs) = (Insertar position x) : insertarTodos xs
	where position   = (len (x:xs)) - (len xs)

minLength :: [PaqueteModificaciones] -> PaqueteModificaciones
minLength []        = []
minLength [x]       = x
minLength (x:y:xys) = if (len x) <= (len y) then minLength (x:xys) else minLength (y:xys)

-- Ejemplos:
-- Main> levenshtein2 "auto" "automata"
-- [Insertar 4 'a',Insertar 4 't',Insertar 4 'a',Insertar 4 'm']

-- Ejercicio 8/8
agregarVersion :: String -> Archivo -> Archivo
agregarVersion [] a           = a 
agregarVersion s ArchivoVacio = NuevaVersion (insertarTodos s) ArchivoVacio
agregarVersion s a            = NuevaVersion modificaciones a
	where
		modificaciones = levenshtein2 (obtenerUltimaVersion a) s

-- Ejemplos:
-- Main> agregarVersion "dato" archivo2
-- Archivo: dato

-- Funciones provistas por la c\'atedra

len :: [a] -> Integer
len xs = fromIntegral (length xs)

-- Archivos

archivo1 = NuevaVersion [Insertar 0 'd', Insertar 1 'a', Insertar 2 't',Insertar 3 'o'] ArchivoVacio

archivo2 = NuevaVersion [Insertar 0 'd'] archivo1
