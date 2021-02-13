 --- Autores:
--  - Denylson Romero
--  - Daniel Marin

-- Ejercicio 0
-- Estructura de datos:
data ArbolMB a = Vacio
	| RamaM a (ArbolMB a)
	| RamaB a (ArbolMB a) (ArbolMB a)

-- definicion por gchi
-- data ArbolMB a = Vacio | RamaM a (ArbolMB a) | RamaB a (ArbolMB a) (ArbolMB a) deriving (Show, Eq, Ord)
-- Usamos Show para probar con ejemplos por ghci.

-- a)
-- RamaM :: a -> ArbolMB a -> ArbolMB a
-- RamaB :: a -> ArbolMB a -> ArbolMB a -> ArbolMB a
-- Vacio :: ArbolMB a

-- b)
-- transformarRamaM :: a -> b -> b
-- transformarRamaB :: a -> b -> b -> b
-- transfromarVacio :: b

-- c)
plegarArbolMB :: b -> (a -> b -> b) -> (a -> b -> b -> b) -> ArbolMB a -> b

plegarArbolMB transVacio transRamaM transRamaB = plegar
	where
		plegar Vacio          = transVacio 
		plegar (RamaM x y)    = transRamaM x (plegar y)
		plegar (RamaB x y z)  = transRamaB x (plegar y) (plegar z)
-- por ghci
-- plegarArbolMB :: b -> (a -> b -> b) -> (a -> b -> b -> b) -> ArbolMB a -> b; plegarArbolMB transVacio transRamaM transRamaB = plegar where plegar Vacio = transVacio; plegar (RamaM x y) = transRamaM x (plegar y); plegar (RamaB x y z) = transRamaB x (plegar y) (plegar z)


-- d)
sumarArbolMB :: (Num a) => ArbolMB a -> a
sumarArbolMB = plegarArbolMB transVacio transRamaM transRamaB
	where
		transVacio = 0
		transRamaM = (\x y -> x + y)
		transRamaB = (\x y z -> x + y + z)

-- por ghci 
-- sumarArbolMB :: (Num a) => ArbolMB a -> a; sumarArbolMB = plegarArbolMB transVacio transRamaM transRamaB where transVacio = 0; transRamaM = (\x y -> x +  y); transRamaB = (\x y z -> x + y + z)

-- e) 
aplanarArbolMB :: ArbolMB a -> [a]
aplanarArbolMB = plegarArbolMB transVacio transRamaM transRamaB 
	where
		transVacio = []
		transRamaM = (\x y -> x : y)
		transRamaB = (\x y z -> x : y ++ z)

-- por ghci
-- aplanarArbolMB :: ArbolMB a -> [a]; aplanarArbolMB = plegarArbolMB transVacio transRamaM transRamaB where transVacio = []; transRamaM = (\x y -> x : y); transRamaB = (\x y z -> x : y ++ z)


-- f) Falta
analizarArbolMB :: (Ord a) => ArbolMB a -> Maybe (a, a, Bool)
analizarArbolMB = plegarArbolMB transVacio transRamaM transRamaB
	where
		transVacio = 
		transRamaM = 
		transRamaB =



-- g) Debe tomar n funciones como argumento, es decir, una funcion por cada constructor
-- diferente (n). El valor de tipo Gen a que se desea plegar, se puede recibir implicitamente en la
-- funcion plegarGen, de manera similar a la funcion plegarArbolMB. 

-- h) La funcion equivalente en el preludio de Haskell es foldrl, ya que tiene una firma e implementacion
-- similar a la de plegarArbolMB, ademas de que foldrl toma un solo argumento al igual que plegarArbolMB, 
-- siendo estos una lista y un arbol, respectivamente.

-- i) Enum es el typeclass que incluye esa estructura ya que la definicion dada es la definicion de una lista en haskell, y
-- Enum permite operaciones sobre listas.
-- Falta mas


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
-- fiboIndex :: Integer -> Maybe Integer; fiboIndex n | n `elem` fibLista = Just $ toInteger $ 1 + length fibLista | otherwise = Nothing where fib 0 = 0; fib 1 = 1; fib n = fib (n-1) + fib (n-2); fibLista = (takeWhile (<=n) (map fib [2..]))
