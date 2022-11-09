-- Arbol binario de busqueda. Donde los elementos del sub-arbol izq. son
-- menores que la raiz y los elementos del sub-arbol der. son mayores que
-- la raiz

-- definicion del tipo ABB polimorfico con constructores: vacio | raiz, hd, hi
data ABB a = Null | Nodo a (ABB a) (ABB a) deriving Show


-- funcion que construye un arbol binario de busqueda. Para usar con cada operacion
valor :: ABB Integer
valor = (Nodo 6 ( Nodo 5 (Nodo 3 Null Null) (Nodo 4 Null Null) ) (Nodo 9 Null Null) )

--           6
--         /   \
--        5    9
--      /  \
--     3   4


valor1 :: ABB Integer
valor1 = (Nodo 4 ( Nodo 2 (Nodo 1 Null Null) (Nodo 3 Null Null) ) ( Nodo 6 (Nodo 5 Null Null) (Nodo 7 Null Null) ) )

--           4
--         /   \
--        2    6
--      /  \  / \ 
--     1   3 5  7

-- funcion que inserta un elemento en una determinada posicion del arbol.
-- toma un tipo polimorfico, un ABB y devuelve un ABB.
inserta :: (Ord a) => a -> ABB a -> ABB a
inserta x Null = Nodo x Null Null
inserta x (Nodo r hi hd) | x == r = Nodo r hi hd
                         | x < r = Nodo r (inserta x hi) hd
                         | x > r = Nodo r hi (inserta x hd)



-- verifica si un elemento pertenece a un arbol.
pertenece :: (Ord a) => a -> ABB a -> Bool
pertenece x Null = False
pertenece x (Nodo r hi hd) | x == r = True
                           | x < r = pertenece x hi
                           | x > r = pertenece x hd


-- elimina un elemento dado del arbol.
elimina :: (Ord a) => a -> ABB a -> ABB a
elimina x Null = Null
elimina x (Nodo r hi Null) | x == r = hi
elimina x (Nodo r Null hd) | x == r = hd
elimina x (Nodo r hi hd) | x < r = Nodo r (elimina x hi) hd
                         | x > r = Nodo r hi (elimina x hd)
                         | x == r = Nodo x' hi (elimina x' hd)
                                       where x' = minimo hd   -- el minimo elemento del sub-arbol derecho pasa a ser la raiz del arbol.


-- busca el minimo valor del sub-arbol derecho, se utiliza dentro de funcion elimina.
minimo :: Ord a => ABB a -> a
minimo (Nodo r Null _) = r -- raiz, hi (vacio), no importa que tenga hd
minimo (Nodo _ hi _) = minimo hi -- no importa que tenga raiz, hi, 
                                -- no importa que tenga hd
