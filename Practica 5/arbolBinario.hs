-- Implementacion de un arbol binario con sus operaciones

-- Se crea el tipo BinTree donde a es polimorfico.
-- Constructores: Vacio | raiz   hijo izq   hijo der
data BinTree a = Null | Node a (BinTree a) (BinTree a) 
 deriving (Show, Eq)

-- Manera de usarlo por terminal
valor1 :: (Num a) => BinTree a
valor1 = (Node 9 (Node 10 (Node 7 Null Null) Null) (Node 20 Null Null))

--           9 
--         /   \ 
--       10    20    
--     /   \  /  \
--    7    N N   N
--   / \      
--  N  N    

-- Manera de usarlo por terminal
valor2 :: (Num a) => BinTree a
valor2 = (Node 0 (Node 3 (Node 4 Null Null) (Node 8 Null Null)) (Node 7 Null (Node 9 Null Null)))

--            0 
--         /     \ 
--       3       7    
--     /   \   /  \
--    4    8  N   9
--   / \  / \    / \
--  N  N N  N   N  N


-- Cantidad de nodos de un arbol
size :: BinTree a -> Int
size Null = 0
size (Node r hi hd) = 1 + size hi + size hd

-- Altura de un arbol, desde la raiz hasta el nodo con el
-- recorrido mas largo
altura :: BinTree a -> Int
altura Null = 0
altura (Node r hi hd) = 1 + max (altura hi) (altura hd)

-- Dice si un arbol esta completo
full :: BinTree a -> Bool
full Null = True
full (Node r hi hd) = full hi && full hd && (altura hi == altura hd)

-- Recorridos de un arbol.

-- Pre-orden:
-- raiz
-- recorrido recursivo de subarbol izq
-- recorrido recursivo de subarbol der
preOrden :: BinTree a -> [a]
preOrden Null = []
preOrden (Node r hi hd) = [r] ++ preOrden hi ++ preOrden hd

-- In-orden:
-- recorrido recursivo de subarbol izq
-- raiz
-- recorrido recursivo de subarbol der
inOrden :: BinTree a -> [a]
inOrden Null = []
inOrden (Node r hi hd) = inOrden hi ++ [r] ++ inOrden hd

-- Post-orden:
-- recorrido recursivo de subarbol izq
-- recorrido recursivo de subarbol der
-- raiz
postOrden :: BinTree a -> [a]
postOrden Null = []
postOrden (Node r hi hd) = postOrden hi ++ postOrden hd ++ [r]
