-- Ejercicio 0
-- Autores:
--  - Denylson Romero
--  - Daniel Marin

data ArbolMB a = Vacio
  | RamaM a (ArbolMB a)
  | RamaB a (ArbolMB a) (ArbolMB a)

-- a)

vacio :: ArbolMB a -> Bool

ramaM :: a -> ArbolMB a -> ArbolMB a

ramaB :: a -> ArbolMB a -> ArbolMB a -> ArbolMB a

-- b)

transformarVacio :: b -> Bool

transformarRamaM :: a -> b -> b

transformarRamaB :: a -> b -> b -> b

-- c)

plegarArbolMB :: (b -> Bool) -> (a -> b -> b) -> (a -> b -> b -> b) -> ArbolMB a -> b
plegarArbolMB transVacio transRamaM transRamaB = plegar
  where
    plegar Vacio = transVacio
    plegar (RamaM x y) = transRamaM x (plegar y)
    plegar (RamaB x y z) = transRamaB x (plegar y) (plegar z)

-- d)

sumarArbolMB :: (Num a) => ArbolMB a -> a
sumarArbolMB = plegarArbolMB transVacio transRamaM transRamaB
  where
    transVacio = ¿?
    transRamaM = ¿?
    transRamaB = ¿?

-- e)

aplanarArbolMB :: ArbolMB a -> [a]
aplanarArbolMB = plegarArbolMB transVacio transRamaM transRamaB
  where
    transVacio = ¿?
    transRamaM = ¿?
    transRamaB = ¿?

-- f)

analizarArbolMB :: (Ord a) => ArbolMB a -> Maybe (a, a, Bool)
analizarArbolMB = plegarArbolMB transVacio transRamaM transRamaB
  where
    transVacio = ¿?
    transRamaM = ¿?
    transRamaB = ¿?

-- g)

-- h)

-- i)

