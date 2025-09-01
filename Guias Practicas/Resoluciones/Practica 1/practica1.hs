import Data.Char (ord, chr)
import Data.Foldable (Foldable(fold))
--EJERCICIO 3

--i. Redefinir usando foldr las funciones sum, elem, (++), filter y map.

sum'::Num a=>[a]->a
sum' = foldr (+) 0

elem'::Eq a=>a->[a]->Bool
elem' e = foldr (\x r ->x==e || r) False

filter'::(a->Bool)->[a]->[a]
filter' f = foldr (\x r->if f x then x:r else r) []

map'::(a->b)->[a]->[b]
map' f = foldr (\x r->f x:r) []

{-
ii. Definir la función mejorSegún :: (a -> a -> Bool) -> [a] -> a, que devuelve el máximo elemento
de la lista según una función de comparación, utilizando foldr1. Por ejemplo, maximum = mejorSegún
(>).
-}
mejorSegun:: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x r->if f x r then x else r) 

{-
iii. Definir la función sumasParciales :: Num a => [a] -> [a], que dada una lista de números devuelve
otra de la misma longitud, que tiene en cada posición la suma parcial de los elementos de la lista original
desde la cabeza hasta la posición actual. Por ejemplo, sumasParciales [1,4,-1,0,5] ; [1,5,4,4,9].
-}

sumasParciales :: Num a => [a] -> [a]
sumasParciales = reverse.foldl (\ac x-> if null ac then [x] else head ac +x:ac) []

{-
iv. Definir la función sumaAlt, que realiza la suma alternada de los elementos de una lista. Es decir, da como
resultado: el primer elemento, menos el segundo, más el tercero, menos el cuarto, etc. Usar foldr.
-}

sumaAlt::Num a=>[a]->a
sumaAlt = sum.foldr (\x r->if null r then [x] else x:map (0-) r) []

{-
v. Hacer lo mismo que en el punto anterior, pero en sentido inverso (el último elemento menos el anteúltimo,
etc.). Pensar qué esquema de recursión conviene usar en este caso.
-}
sumaAltInv::Num a=>[a]->a
sumaAltInv = sumaAlt.reverse

--EJERCICIO 4

{-
i. Definir la función permutaciones :: [a] -> [[a]], que dada una lista devuelve todas sus permutacio-
nes. Se recomienda utilizar concatMap :: (a -> [b]) -> [a] -> [b], y también take y drop.
-}

permutaciones::[a]->[[a]]
permutaciones = foldr (concatMap.combinar) [[]]
    where combinar x r = concatMap (\n->[take n r++[x]++drop n r]) [0..length r]

{-ii. Definir la función partes, que recibe una lista L y devuelve la lista de todas las listas formadas por los
mismos elementos de L, en su mismo orden de aparición.
Ejemplo: partes [5, 1, 2] → [[], [5], [1], [2], [5, 1], [5, 2], [1, 2], [5, 1, 2]]
(en algún orden).-}

partes::[a]->[[a]]
partes = foldr (\x r->concatMap (\l->[l,x:l]) r) [[]]

{-
iii. Definir la función prefijos, que dada una lista, devuelve todos sus prefijos.
Ejemplo: prefijos [5, 1, 2] → [[], [5], [5, 1], [5, 1, 2]]
-}

prefijos::[a]->[[a]]
prefijos xs= [take n xs | n<-[0..length xs]]

{-
iv. Definir la función sublistas que, dada una lista, devuelve todas sus sublistas (listas de elementos que
aparecen consecutivos en la lista original).
Ejemplo: sublistas [5, 1, 2] → [[], [5], [1], [2], [5, 1], [1, 2], [5, 1, 2]]
(en algún orden).
-}
recr::(a->[a]->b->b)->b->[a]->b
recr f z [] = z
recr f z (x:xs) = f x xs (recr f z xs)

sublistas::[a]->[[a]]
sublistas = ([]:).recr (\x xs r->[take n (x:xs) | n<-[1..length (x:xs)]]++r) []

--EJERCICIO 5

{-
Considerar las siguientes funciones:

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs
                                    then [x]
                                    else x : elementosEnPosicionesPares (tail xs)

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys
                            then x : entrelazar xs []
                            else x : head ys : entrelazar xs (tail ys)

Indicar si la recursión utilizada en cada una de ellas es o no estructural. Si lo es, reescribirla utilizando foldr.
En caso contrario, explicar el motivo.

i. No es recursion estructural porque se realiza un llamado sobre la cola con una funcion distinta de elementosEnPosicionesPares al hacer (null xs) y (tail xs)

ii. Es recursion estructural pues el caso base devuelve un valor fijo y el caso recursivo solo llama a la funcion y a la cola de la lista al hacer (entrelazar xs). Veamos entonces su definicion con foldr
-}

entrelazar :: [a] -> [a] -> [a]
entrelazar = foldr (\x r ys->if null ys then x:r [] else x:head ys:r (tail ys) ) id

--EJERCICIO 6

{-
a. Definir la función sacarUna :: Eq a => a -> [a] -> [a], que dados un elemento y una lista devuelve el
resultado de eliminar de la lista la primera aparición del elemento (si está presente).
-}

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\x xs r-> if e==x then xs else x:r) []


{-
b. Explicar por qué el esquema de recursión estructural (foldr) no es adecuado para implementar la función
sacarUna del punto anterior.

El esquema de recursion estructural no es adecuado para implementar sacarUna dado que en el caso de que se encuentre el elemento deberiamos devolver lo que resta de la lista (la cola) y foldr no permite esto pues la recursion estructural no permite el acceso a la cola
-}



{-
c. Definir la función insertarOrdenado :: Ord a => a -> [a] -> [a] que inserta un elemento en una lista
ordenada (de manera creciente), de manera que se preserva el ordenamiento.
-}

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr (\x xs r->if e<x then e:x:xs else x:r) [e]

--EJERCICIO 7

{-i. mapPares, una versión de map que toma una función currificada de dos argumentos y una lista de pares
de valores, y devuelve la lista de aplicaciones de la función a cada par. Pista: recordar curry y uncurry.-}

mapPares::(a->b->c)->[(a,b)]->[c]
mapPares = map.uncurry



{-ii. armarPares, que dadas dos listas arma una lista de pares que contiene, en cada posición, el elemento
correspondiente a esa posición en cada una de las listas. Si una de las listas es más larga que la otra,
ignorar los elementos que sobran (el resultado tendrá la longitud de la lista más corta). Esta función en
Haskell se llama zip. Pista: aprovechar la currificación y utilizar evaluación parcial.-}

armarPares::[a]->[b]->[(a,b)]
armarPares= foldr (\x r zs->case zs of
                            []->[]
                            (z':zs')->(x,z'):r zs') (const [])

{-iii. mapDoble, una variante de mapPares, que toma una función currificada de dos argumentos y dos listas
(de igual longitud), y devuelve una lista de aplicaciones de la función a cada elemento correspondiente de
las dos listas. Esta función en Haskell se llama zipWith.-}

mapDoble::(a->b->c)->[a]->[b]->[c]
mapDoble f xs ys = mapPares f (armarPares xs ys)

--EJERCICIO 8

{-
i. Escribir la función sumaMat, que representa la suma de matrices, usando zipWith. Representaremos una
matriz como la lista de sus filas. Esto quiere decir que cada matriz será una lista finita de listas finitas,
todas de la misma longitud, con elementos enteros. Recordamos que la suma de matrices se define como
la suma celda a celda. Asumir que las dos matrices a sumar están bien formadas y tienen las mismas
dimensiones.
sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
-}

sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat  = zipWith (zipWith (+)) 

{-
ii. Escribir la función trasponer, que, dada una matriz como las del ítem i, devuelva su traspuesta. Es decir,
en la posición i, j del resultado está el contenido de la posición j, i de la matriz original. Notar que si la
entrada es una lista de N listas, todas de longitud M , la salida debe tener M listas, todas de longitud N .
trasponer :: [[Int]] -> [[Int]]
-}
nesimo :: Int -> [Int] -> Int
nesimo n xs = foldr (\x r k -> if k == 0 then x else r (k-1)) (const 0) xs n 

transponer::[[Int]]->[[Int]]
transponer mat = [concatMap ((:[]).nesimo n) mat|n<-[0..length (head mat)-1]]

--EJERCICIO 9

{-
i. Definir y dar el tipo del esquema de recursión foldNat sobre los naturales. Utilizar el tipo Integer de
Haskell (la función va a estar definida sólo para los enteros mayores o iguales que 0).
ii. Utilizando foldNat, definir la función potencia.
-}

foldNat::(Integer->b->b)->b->Integer->b
foldNat cR cB 0 = cB
foldNat cR cB n = cR n (foldNat cR cB (n-1))

mul::Integer->Integer->Integer
mul a = foldNat (\x r->a+r) 0

pot::Integer->Integer->Integer
pot b = foldNat (\x r->mul b r) 1

{-
Ejercicio 10
i. Definir la función genLista :: a -> (a -> a) -> Integer -> [a], que genera una lista de una canti-
dad dada de elementos, a partir de un elemento inicial y de una función de incremento entre los elementos
de la lista. Dicha función de incremento, dado un elemento de la lista, devuelve el elemento siguiente.
-}



genLista :: a -> (a -> a) -> Int -> [a]
genLista ini f 0 = [] 
genLista ini f cant = ini:genLista (f ini) f (cant-1) 

--genLista 'a' (\x->chr (ord x+2)) 3

{-
ii. Usando genLista, definir la función desdeHasta, que dado un par de números (el primero menor que el
segundo), devuelve una lista de números consecutivos desde el primero hasta el segundo.
-}

desdeHasta::Int->Int->[Int]
desdeHasta a b = genLista a (1+) (b-a+1)

--EJERCICIO 11

{-
Definir el esquema de recursión estructural para el siguiente tipo:
data Polinomio a = X
| Cte a
| Suma (Polinomio a) (Polinomio a)
| Prod (Polinomio a) (Polinomio a)
Luego usar el esquema definido para escribir la función evaluar :: Num a => a -> Polinomio a -> a
que, dado un número y un polinomio, devuelve el resultado de evaluar el polinomio dado en el número dado.
-}

data Polinomio a = X | Cte a | Suma (Polinomio a) (Polinomio a) | Prod (Polinomio a) (Polinomio a)

foldPol::(a->b)->b->(b->b-> b)->(b->b-> b)->Polinomio a->b
foldPol cCte cVar cSuma cProd  (Cte a)= cCte a
foldPol cCte cVar cSuma cProd  X= cVar
foldPol cCte cVar cSuma cProd  (Prod a b)= cProd (foldPol cCte cVar cSuma cProd a) (foldPol cCte cVar cSuma cProd b)
foldPol cCte cVar cSuma cProd  (Suma a b)= cSuma (foldPol cCte cVar cSuma cProd a) (foldPol cCte cVar cSuma cProd b)

evaluar::Num a=>a->Polinomio a->a
evaluar n = foldPol id n (+) (*)
--evaluar 3 (Suma (Prod X X) (Suma (Prod (Cte 1) X) (Cte 1)))= f(3) --->f(x)= x^2+x+1

{-
Ejercicio 12 ⋆
Considerar el siguiente tipo, que representa a los árboles binarios:
data AB a = Nil | Bin (AB a) a (AB a)

i. Usando recursión explícita, definir los esquemas de recursión estructural (foldAB) y primitiva (recAB), y
dar sus tipos.
-}
data AB a = Nil | Bin (AB a) a (AB a)

foldAB::(b->a->b->b)->b->AB a->b
foldAB cBin cNil Nil = cNil
foldAB cBin cNil (Bin i r d) = cBin (rec i) r (rec d)
    where rec= foldAB cBin cNil

recAB::(a->AB a->AB a->b->b->b)->b->AB a->b
recAB cBin cNil Nil = cNil
recAB cBin cNil (Bin i r d) = cBin r i d (rec i) (rec d)
    where rec= recAB cBin cNil


{-
ii. Definir las funciones esNil, altura y cantNodos (para esNil puede utilizarse case en lugar de foldAB
o recAB).
-}
esNil::AB a->Bool
esNil Nil = True
esNil _ = False

altura::AB a->Int
altura = foldAB (\ri r rd->1+max ri rd) 0

cantNodos::AB a->Int
cantNodos = foldAB (\ri r rd->ri+rd+1) 0

{-
iii. Definir la función mejorSegún :: (a -> a -> Bool) -> AB a -> a, análoga a la del ejercicio 3, para árboles.
Se recomienda definir una función auxiliar para comparar la raíz con un posible resultado de la recursión
para un árbol que puede o no ser Nil.
-}

mejorSegunAB :: (a -> a -> Bool) -> AB a -> Maybe a
mejorSegunAB f = foldAB (\ri r rd->case (ri,rd) of
    (Nothing,Nothing)->Just r
    (Just i,Nothing)->if f r i then Just r else Just i
    (Nothing,Just d)->if f r d then Just r else Just d
    (Just i,Just d)->if f r i && f r d then Just r else (if f i d then Just i else Just d) ) Nothing

{-
iv. Definir la función esABB :: Ord a => AB a -> Bool que chequea si un árbol es un árbol binario de búsqueda.
Recordar que, en un árbol binario de búsqueda, el valor de un nodo es mayor o igual que los valores que
aparecen en el subárbol izquierdo y es estrictamente menor que los valores que aparecen en el subárbol
derecho.
-}

esABB :: Ord a => AB a -> Bool
esABB = recAB (\r i d ri rd->caso (>=) r i&& caso (<) r d&& ri && rd) True
    where caso f r ab= case mejorSegunAB f ab of
            Nothing->True
            (Just x)->f r x