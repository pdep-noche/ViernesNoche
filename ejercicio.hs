
doble :: Integer -> Integer
doble nro = nro * 2


calcular :: (Integer, Integer) ->(Integer, Integer)
calcular (nro1, nro2) = (duplicaPar nro1, sumaUnoImpar nro2)

duplicaPar nro | even nro = doble nro
               | otherwise =nro

sumaUnoImpar nro |odd nro = siguiente nro
                 | otherwise = nro

siguiente nro = nro + 1

and' :: Bool -> Bool -> Bool
and' val otroVal  | val = otroVal
                  |otherwise = False

and'' :: Bool -> Bool -> Bool
and'' True val = val
and'' _ _ = False

or':: Bool -> Bool -> Bool
or' True _ = True
or' _ val = val

or'' False  False = False
or'' _ _ = True

or''' False val = val
or''' _ _ = True


data Empleado = Comun {nombre :: String, sueldoBasico:: Integer} | 
        Jefe {sueldoBasico :: Integer, cantACargo :: Integer, nombre :: String} deriving Show

sonia :: Empleado
sonia = Jefe 15000 3 "Sonia"

sueldo (Comun _ basico) = basico
sueldo (Jefe basico cantidad _) =  basico + plus cantidad

plus cantidad = cantidad * 500

