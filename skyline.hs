-- Tipos de datos que se van a utilizar en el programa

type Edificio = (Int,Int,Int)
type Coordenada = (Int,Int)
type Silueta = [Coordenada]

------------------------------------------------------------------
-- Resolución de la práctica --
------------------------------------------------------------------

-- Función principal llamada resuelve. A partir de una lista de edificios,
-- implementa el algoritmo mediante un Divide y Vencerás y devuelve la Silueta
-- Se debe implementar mediante patrones y recursividad. La función debe hacer uso 
-- de otras dos funciones: una que divide la lista Edificio en dos sublistas, 
-- para llamarse recursivamente con las subsistas, y otra que une los resultados
-- de resolver las dos sublistas separadamente.

resuelve :: [Edificio] -> Silueta
resuelve (e:[])=siluetadeedificio e
resuelve es=une (resuelve as) (resuelve bs)
  where (as,bs) = divide es


-- Función siluetadeedificio, que transforma un único Edificio en su Silueta correspondiente
-- Se utiliza en el caso base del algoritmo

siluetadeedificio :: Edificio -> Silueta
siluetadeedificio (ini,fin,alt) = [(ini,alt),(fin,0)]

-- Función divide, que dado un problema (en forma de lista de Edificios) lo divide en
-- dos subproblemas cuyo tamaño es la mitad del problema original (divide y vencerás)
-- Parte de una lista y obtiene una tupla con dos listas, que son el resultado de dividir la lista de entrada en dos sublistas.

divide :: [a] -> ([a],[a])
divide xs = (take (length (xs)`div`2) xs,drop (length (xs)`div`2) xs)

-- Función de unión que dadas dos siluetas las une para formar una única.
-- Debe tenerse en cuenta que una vez unidas las dos siluetas hay que chequear
-- que dos coordenadas consecutivas no tengan la misma altura.


une :: Silueta -> Silueta -> Silueta
une as bs = compact 0 0 alturaFinal
    where alturas1=altura as
          alturas2=altura bs
          alturaFinal=maximoListas alturas1 alturas2
          
          
compact :: Int -> Int -> [Int] -> Silueta
compact i anterior [] = [(i,anterior)]
compact i anterior (x1:xs)
    | anterior/=x1 = [(i,x1)]++(compact (i+1) x1 xs)
    | otherwise = compact (i+1) anterior xs
          
          
altura :: Silueta -> [Int]
altura xs = [snd(anterior xs i) | i<-[0..size]]
    where size = fst(last xs)

maximoListas :: [Int] -> [Int] -> [Int]
maximoListas [] [] = []
maximoListas (x:xs) [] = [x]++maximoListas xs []
maximoListas [] (y:ys) = [y]++maximoListas [] ys
maximoListas (x:xs) (y:ys)
    | x > y = [x]++maximoListas xs ys
    | otherwise = [y]++maximoListas xs ys


------------------------------------------------------------
-- Algoritmo para imprimir en pantalla la silueta --
------------------------------------------------------------
-- En primer lugar, se convierte la lista de coordenadas dada por la Silueta 
-- en una lista de alturas para cada coordenada x.
-- En segundo lugar, se calcula la altura máxima de la Silueta, a partir de la lista anterior.
-- En tercer lugar, se genera la línea de asteriscos para la altura máxima 
-- y descendemos hasta llegar a la altura cero. 
-- Para dibujar cada línea, en cada coordenada x se visualiza un asterisco, 
-- si la altura de la silueta en esa coordenada es mayor o igual que la altura de la línea 
-- que estamos dibujando y un espacio en blanco, si no se verifica lo anterior.
-- Finalmente, en la altura cero se visualiza una línea de guiones para indicar el suelo. ▓

imprime :: Silueta -> IO()
imprime ss = putStr (unlines (toString ss))

toString :: Silueta -> [[Char]]
toString ss = [linean line | line <- [1..maxAlto]] ++ [['-' | i <- [1..maxLargo]]]
    where maxAlto = maximum (map snd ss)
          maxLargo = maximum (map fst ss)
          linean y = [obtenerCaracter x y | x <- [1..maxLargo]]
          obtenerCaracter x y
            | snd(anterior ss x) > (maxAlto - y) = '▓'
            | otherwise = '🌁'

anterior :: Silueta -> Int -> Coordenada
anterior ss x
    | anteriores == [] = (0,0)
    | otherwise = last(anteriores)
    where anteriores = filter (\p -> fst(p) < x) ss
