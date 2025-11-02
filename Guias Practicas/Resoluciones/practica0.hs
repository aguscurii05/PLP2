--Ejercicio 2

--a
valorAbsoluto::Float->Float
valorAbsoluto x =if x>0 then x else -x

--b
bisiesto::Int->Bool
bisiesto año = if mod año 100/=0 
                then mod año 4==0 
                else mod año 400==0

--c
factorial::Int->Int
factorial 0=1
factorial n=n*factorial (n-1)

--d
esPrimo::Int->Int->Bool
esPrimo num div = if num==div then True else mod num div/=0 && esPrimo num (div+1)

contarDivPrimos::Int->Int->Int
contarDivPrimos num 2 = if mod num 2==0 then 1 else 0
contarDivPrimos num div | mod num div ==0 && esPrimo div 2 = 1+ rec
                        | otherwise = rec
                        where rec = contarDivPrimos num (div-1)

cantDivPrimos::Int->Int
cantDivPrimos num = contarDivPrimos num num

--Ejercicio 3

--a
inverso :: Float-> Maybe Float
inverso 0 = Nothing
inverso num = Just (1/num)

--b

aEntero::Either Int Bool ->Int
aEntero (Left a) = a
aEntero (Right b) = if b then 1 else 0

--Ejercicio 4

--a

borrar::Char->String->String
borrar _ [] = []
borrar c (x:xs) = if c==x then borrar c xs else x:borrar c xs

limpiar::String->String->String
limpiar [] pal = pal
limpiar (c:cs) pal = limpiar cs (borrar c pal)

--b

sumLista::[Float]->Float
sumLista []=0
sumLista (x:xs) = x+sumLista xs

prom::[Float]->Float
prom xs = (sumLista xs)/fromIntegral (length xs)

difs::[Float]->Float->[Float]
difs [] _ =[]
difs (x:xs) prom = (x-prom):difs xs prom

difsPromedio::[Float]->[Float]
difsPromedio lista = difs lista (prom lista)

--c

todosIguales::[Int]->Bool
todosIguales [x] = True
todosIguales (x:xs) = x==head xs && todosIguales xs

--Ejercicio 5

data AB a = Nil | Bin (AB a) a (AB a) deriving (Show)

--a

vacioAB::AB a->Bool
vacioAB Nil = True
vacioAB _ = False

--b

negacionAB::AB Bool->AB Bool
negacionAB Nil = Nil
negacionAB (Bin i r d) = Bin (negacionAB i) (not r) (negacionAB d)

--c

productoAB::AB Int->Int
productoAB Nil = 1
productoAB (Bin i r d) = r*(productoAB i)*(productoAB d)