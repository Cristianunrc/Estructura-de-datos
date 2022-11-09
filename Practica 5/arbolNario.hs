-- Arboles n-arios.

-- Se crea el tipo de dato Ntree y polimorfico, un constructor vacio y otro
-- con la raiz y n hijos.
data NTree a = Null | Nodo a [NTree a]  deriving Show


-- funcion para usar con las operaciones del arbol.
valor :: NTree Integer -- devuelve un arbol n-ario de Integer
valor = Nodo 40 [a1, a2, a3]
  where 
    a1 = Nodo 22 [hoja 15, hoja 12]
    a2 = hoja 35
    a3 = Nodo 52 [hoja 33]

--		 40
--	      /  |  \ 
--	    22  35  52
--         / \      /
--       15  12    33

-- funcion que se usa dentro de la funcion valor.
hoja :: a -> NTree a
hoja x = Nodo x []

-- devuelve la raiz.
raiz :: NTree a -> a
raiz Null = error "arbol vacio."
raiz (Nodo r _) = r

-- devuelve el tamaño de un arbol
size :: NTree a -> Integer
size Null = 0
size (Nodo _ xs) = 1 + sum (map size xs) -- map toma el tamaño (la funcion) y la aplica a cada elemento de la lista, luego se hace la sumatorio de los elementos


-- devuelve la altura del arbol, desde la raiz hasta la hoja con el recorrido
-- mas largo.
altura :: NTree a -> Integer
altura Null = 0
altura (Nodo _ []) = 1 -- no importa que tengo la raiz y vacio
altura (Nodo _ xs) = 1 + maximum (map altura xs)
