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
aplicarModificacion (x:xs) (Borrar 1) = xs 
aplicarModificacion (x:xs) (Borrar n) = x: (aplicarModificacion xs (Borrar (n-1)))
aplicarModificacion  [] (Insertar 0 x) = [x]
aplicarModificacion (x:xs) (Insertar 0 y) = (y:x:xs)
aplicarModificacion (x:xs) (Insertar n y) = x: (aplicarModificacion xs (Insertar (n-1) y))
aplicarModificacion (x:xs) (Substituir n y) = aplicarModificacion ( aplicarModificacion  (x:xs)(Borrar n)) (Insertar (n-1) y)
                                        
--aplicarModificacion = error "Implementar!!! (ejercicio 1)"

-- Ejemplos:
-- Main> aplicarModificacion "d" (Insertar 1 'a')
-- "da"
-- Main> aplicarModificacion "d" (Insertar 0 'a')
-- "ad"
-- Main> aplicarModificacion "dato" (Borrar 1)
-- "ato"

-- Ejercicio 2/8
aplicarPaqueteModificaciones :: String -> PaqueteModificaciones -> String
aplicarPaqueteModificaciones (s:st) [] = (s:st)
aplicarPaqueteModificaciones [] (p:pm) = aplicarPaqueteModificaciones (aplicarModificacion [] p) pm
aplicarPaqueteModificaciones (s:st) (p:pm) = aplicarPaqueteModificaciones (aplicarModificacion (s:st) p) pm 
                                               
--aplicarPaqueteModificaciones = error "Implementar!!! (ejercicio 2)"

-- Ejemplos:
-- Main> aplicarPaqueteModificaciones "dato" [Substituir 1 'p', Insertar 4 's']
-- "patos"

-- Ejercicio 3/8
obtenerUltimaVersion :: Archivo -> String
obtenerUltimaVersion ArchivoVacio = "Archivo Vacio"
obtenerUltimaVersion (NuevaVersion pm ArchivoVacio) = aplicarPaqueteModificaciones ([]) pm
obtenerUltimaVersion (NuevaVersion pm nv) = aplicarPaqueteModificaciones (obtenerUltimaVersion nv) pm 

--obtenerUltimaVersion = error "Implementar!!! (ejercicio 3)"

-- Ejemplos: (ver def. archivo1 y archivo2 abajo)
-- Main> obtenerUltimaVersion archivo1
-- "dato"
-- Main> obtenerUltimaVersion archivo2
-- "ddato"
 
-- Ejercicio 4/8
cantVersiones :: Archivo -> Integer
cantVersiones ArchivoVacio = 0
cantVersiones (NuevaVersion pm ArchivoVacio) = 1
cantVersiones (NuevaVersion pm nv) = 1 + cantVersiones nv 

--cantVersiones = error "Implementar!!! (ejercicio 4)"

-- Ejemplos:
-- Main> cantVersiones archivo1
-- 1
-- Main> cantVersiones archivo2
-- 2

-- Ejercicio 5/8
obtenerVersion :: Integer -> Archivo -> String
obtenerVersion 0 (_) = "Archivo Vacio"
obtenerVersion 1 (NuevaVersion pm ArchivoVacio) = obtenerUltimaVersion (NuevaVersion pm ArchivoVacio)
obtenerVersion n (NuevaVersion pm nv)| cantVersiones (NuevaVersion pm nv) == n = obtenerUltimaVersion (NuevaVersion pm nv)
                                     | otherwise = obtenerVersion n nv  
--obtenerVersion  = error "Implementar!!! (ejercicio 5)"

-- Ejemplos:
-- Main> obtenerVersion 1 archivo2
-- "dato"

-- Ejercicio 6/8
levenshtein :: String -> String -> Integer --PaqueteModificaciones
levenshtein s1 [] = len s1
levenshtein [] s2 = len s2
levenshtein s1 s2 | min (len(s1)) (len(s2))== 0 = max (len(s1)) (len(s2))
                  | otherwise = minimum [a, b, c] 
          where
          a = (levenshtein (init s1) s2) + 1
          b = (levenshtein s1 (init s2)) + 1 
          c = (levenshtein (init s1) (init s2)) + (igualesUltimos s1 s2)   


igualesUltimos :: String -> String-> Integer
igualesUltimos (_:x) (_:y)| y == x = 0
                          | otherwise = 1  
--levenshtein = error "Implementar!!! (ejercicio 6)"

-- Ejemplos:
-- Main> levenshtein "auto" "automata"
-- 4

-- Ejercicio 7/8
levenshtein2 :: String -> String -> PaqueteModificaciones
levenshtein2 = error "Implementar!!! (ejercicio 7)"

-- Ejemplos:
-- Main> levenshtein2 "auto" "automata"
-- [Insertar 4 'a',Insertar 4 't',Insertar 4 'a',Insertar 4 'm']

-- Ejercicio 8/8
agregarVersion :: String -> Archivo -> Archivo
agregarVersion = error "Implementar!!! (ejercicio 8)"

-- Ejemplos:
-- Main> agregarVersion "dato" archivo2
-- Archivo: dato

-- Funciones provistas por la c\'atedra

len :: [a] -> Integer
len xs = fromIntegral (length xs)

-- Archivos

archivo1 = NuevaVersion [Insertar 0 'd', Insertar 1 'a', Insertar 2 't',Insertar 3 'o'] ArchivoVacio

archivo2 = NuevaVersion [Insertar 0 'd'] archivo1
