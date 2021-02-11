 --- Autores:
--  - Denyl Romero
--  - Daniel Marin

-- Ejercicio 0
-- Estructura de datos:
data ArbolMB a = Vacio
	| RamaM a (ArbolMB a)
	| RamaB a (ArbolMB a) (ArbolMB a)

-- a)
RamaM :: a -> ArbolMB a -> ArbolMB a
RamaB :: a -> ArbolMB a -> ArbolMB a -> ArbolMB a
Vacio :: ArbolMB a

-- b)
transformarRamaM :: a -> b -> b
transformarRamaB :: a -> b -> b -> b
transfromarVacio :: b

-- c)
plegarArbolMB :: b -> (a -> b -> b) -> (a -> b -> b -> b) -> ArbolMB a -> b

plegarArbolMB transVacio transRamaM transRamaB = plegar
	where
		plegar Vacio          = transVacio 
		plegar (RamaM x y)    = transRamaM x (plegar y)
		plegar (RamaB x y z)  = transRamaB x (plegar y) (plegar z)

-- d) aun no ta listo
sumarArbolMB :: (Num a) => ArbolMB a -> a
sumarArbolMB = plegarArbolMB transVacio transRamaM transRamaB
	where
		transVacio = 0
		transRamaM = RamaM a + sumarArbolMB 
		transRamaB = ¿?


-- Ejercicio 1
fiboIndex :: Integer -> Maybe Integer;
fiboIndex n =
	| n `elem` fibLista = Just $ toInteger $ 1 + length fibLista
	| | otherwise = Nothing 
	where fib 0 = 0
		  fib 1 = 1
		  fib n = fib (n-1) + fib (n-2)
		  fibLista = (takeWhile (<=n) (map fib [2..]))

-- Definicion por ghci:
fiboIndex :: Integer -> Maybe Integer; fiboIndex n | n `elem` fibLista = Just $ toInteger $ 1 + length fibLista | otherwise = Nothing where fib 0 = 0; fib 1 = 1; fib n = fib (n-1) + fib (n-2); fibLista = (takeWhile (<=n) (map fib [2..]))
