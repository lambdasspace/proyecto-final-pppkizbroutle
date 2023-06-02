-- | Módulo para calcular el cierre convexo de un conjunto de discos
module ConvexDiscs where

import Primitives
import Data.List
import Data.Maybe

-- | Función auxiliar utilizada por advance que dado un vector y una
-- lista no vacía de posibles líneas, nos dice si la primera de ellas
-- es la mínima.
isFirstMin :: VectDir -> [Maybe Line] -> Bool
isFirstMin l xs = case head xs of
                    Nothing -> False
                    Just v -> let result = do
                                    a <- foldl1' fun (map (>>= (return . dirAngle l . lineToDir)) xs)
                                    return $ dirAngle l (lineToDir v) == a
                              in if isJust result then fromJust result else False
  where
    fun Nothing y = y
    fun x Nothing = x
    fun (Just x) (Just y) = Just $ min x y

-- | Función merge que dados dos cierres convexos, calcula el cierre
-- convexo de la unión de ambos.
-- Sólo prepara la iteración.

-- merge no se ejecuta recursivamente a diferencia de merge'.  Todos
-- los pasos de la inicialización se ejecutan una única vez y toman a
-- lo más O(n); éstas funciones que toman tiempo lineal son "cut",
-- "last", "length" e "init".  La llamada a función merge' toma tiempo
-- O(n) también; de esta manera merge sigue siendo O(n).
merge :: [Disc] -> [Disc] -> Maybe [Disc]
merge hullP hullQ = do
  let cutP = if length hullP > 1 then cut hullP else hullP
  let cutQ = if length hullQ > 1 then cut hullQ else hullQ
  let hP = if (did (head cutP) == did (last cutP)) && (length cutP > 1) then init cutP else cutP
  let hQ = if (did (head cutQ) == did (last cutQ)) && (length cutQ > 1) then init cutQ else cutQ
  let p = mkInf hP
  let q = mkInf hQ
  cp <- current p
  cq <- current q
  let minY = if (snd (center cp) - radius cp) < (snd (center cq) - radius cq) then
               cp
             else
               cq
  let l = VectDir { p = (fst $ center minY, snd (center minY) - radius minY), dx = -1, dy = 0}
  let lp = paraFromCircle l cp
  let lq = paraFromCircle l cq
  merge' [] hP hQ p q l lp lq

-- | Función auxiliar de merge que realiza las iteraciones que
-- necesarias y regresa una lista con el cierre convexo.
merge' :: [Disc] -> [Disc] -> [Disc] -> InfList Disc -> InfList Disc -> VectDir -> VectDir -> VectDir -> Maybe [Disc]
merge' hS [] [] p q l lp lq = do
  cp <- current p
  cq <- current q
  (hS', _, _, _) <- if dom lp lq then
                      advance (add hS cp) l p q
                    else
                      advance (add hS cq) l q p
  return $ reverse hS'
merge' hS hP hQ p q l lp lq = do
  cp <- current p
  cq <- current q
  (hS', l', x, y) <- if dom lp lq then
                       advance (add hS cp) l p q
                     else
                       advance (add hS cq) l q p
  let (p', q') = if dom lp lq then
                   (x, y)
                 else
                   (y, x)
  cp' <- current p'
  cq' <- current q'
  let (lp', lq') = (paraFromCircle l' cp', paraFromCircle l' cq')
  let (hP', hQ') = case (hP, hQ) of
                     ([], ys) -> ([], remove ys)
                     (xs, []) -> (remove xs, [])
                     (xs, ys) -> if dom lp lq then
                                   (remove xs, ys)
                                 else
                                   (xs, remove ys)
  merge' hS' hP' hQ' p' q' l' lp' lq'

-- | Función advance utilizada por merge' que actualiza la línea L*,
-- los puntos 'x', 'y' y el cierre convexo completo de ser necesario.
advance :: [Disc] -> VectDir -> InfList Disc -> InfList Disc -> Maybe ([Disc], VectDir, InfList Disc, InfList Disc)
advance hS l x y = do -- (hS', lineToDir l', x', y')
  cx <- current x
  cy <- current y
  cnx <- current $ next x
  cny <- current $ next y
  let line1 = tangentFromDiscToDisc cx cy
  let line2 = tangentFromDiscToDisc cx cnx
  let line3 = tangentFromDiscToDisc cy cny
  let line4 = tangentFromDiscToDisc cy cx
  let hS' = if isFirstMin l [line1, line2, line3] then
              if isFirstMin l [line4, line2, line3] then
                cx : (add hS cy) 
              else
                add hS cy
            else
              hS
  (l', x',y') <-
    if isFirstMin l [line2, line3] then do
      l2 <- line2
      return (lineToDir l2, next x, y)
    else
      case line3 of
        Nothing -> return (l, x, y)
        Just l3 -> return (lineToDir l3, x, next y)
  return (hS', l', x', y')

-- | Función auxiliar que recorta el cierrre convexo de ser necesario.

-- En el paso merge' puede ocurrir que nos de correctamente el cierre
-- convexo pero con algunos elementos de más, por lo que para seguir
-- construyendo correctamente los ciclos en cada paso, necesitamos
-- recortar la lista desde la segunda aparición consecutiva de los
-- primeros dos elementos. El resultado va a seguir siendo un cierre
-- convexo ya que dos círculos consecutivos del cierre convexo no
-- pueden aparecer más de una vez en el mismo.

-- Una solución que se me viene a la mente para detectar el momento en
-- el que se empieza a repetir el ciclo es tener una tabla A
-- inicializada en ceros, tal que la entrada A_{ij} tiene valor 1 si y
-- sólo si los círculos i y j fueron añadidos y además el círculo i
-- está justo antes que el círculo j. De esta manera en la iteración
-- de merge podemos ir llenando la tabla y al encontrar un uno romper
-- la iteración. Sin embargo una implementación así en haskell no
-- sería muy eficiente ya que no hay alguna estructura de datos sin el
-- uso de efectos secundarios que nos permita acceder a sus elementos
-- en tiempo O(1); el acercamiento más óptimo (sin usar efectos
-- secundarios) sería simular arreglos con árboles balanceados de
-- rangos, pero su acceso tomaría O(log n), y si es bidimensional
-- O(log^2 n), por lo que merge tomaría O(n log^2 n) en tiempo y el
-- cierre convexo final tomaría O(n log^3 n) :/
cut :: [Disc] -> [Disc]
cut (x:y:xs) = x : y : auxCut x y xs
  where
    auxCut x y [] = []
    auxCut x y [z] = [z]
    auxCut x y (z:w:ws) = if x == z && y == w then
                            []
                          else
                            z : auxCut x y (w:ws)
cut xs = error $ show xs

-- | Función que dada una lista de discos XS y un disco D, regresa la
-- lista D:XS y el primer elemento de XS era distinto a D, XS en otro
-- caso.
add :: [Disc] -> Disc -> [Disc]
add [] d = [d]
add (x:xs) d = if did x == did d then
                 x:xs
               else
                 d:x:xs

-- | Función que de ser posible quita el primer elemento de la lista,
-- de no serlo, nos regresa la lista vacía
remove :: [Disc] -> [Disc]
remove [] = []
remove (_:xs) = xs

-- | Función que nos da el cierre convexo de un conjunto de discos
-- (añade el primer elemento al final si lo necesita)
convexDisc :: [Disc] -> [Disc]
convexDisc xs = case convexHull xs of
                  Nothing -> []
                  Just [] -> []
                  Just [x] -> [x]
                  Just [x,y] -> [x,y]
                  Just ys -> let result = cut ys in if (did (head result) == did (last result)) then result else result ++ [head result]

-- | Función que nos regresa el cierre convexo de un conjunto de
-- discos; puede contener más discos de los necesarios.
convexHull :: [Disc] -> Maybe [Disc]
convexHull [] = Just []
convexHull [x] = Just [x]
convexHull xs = do
  let n = length xs `div` 2
  let p = take n xs
  let q = drop n xs
  hullP <- convexHull p
  hullQ <- convexHull q
  hullP `merge` hullQ

-- | Función que calcula el cierre convexo de un conjunto de discos, y
-- sólo regresa los segmentos de recta que lo componen.
drawHull :: [Disc] -> [Line]
drawHull xs = case convexDisc xs of
                 [] -> []
                 [x] -> []
                 ys -> auxDraw ys
  where
    auxDraw [x] = []
    auxDraw (x:y:xs) = (fromJust $ tangentFromDiscToDisc x y) : auxDraw (y:xs)

-- | Ejemplo de clase
{-
discA = Disc { did = "A", center = (-0.32, 1.56), radius = 1.1260550608207396}
discB = Disc { did = "B", center = (-0.16, -0.22), radius = 1.6401219466856725}
discC = Disc { did = "C", center = (2.82, 2.62), radius = 1.5986244086714048}
discD = Disc { did = "D", center = (4.66, -0.04), radius = 2.2729716232280595}
discE = Disc { did = "E", center = (5.82, 2.68), radius = 2.6768638366566195}
discs = [discA, discB, discC, discD, discE]
-}
{-
ghci> map did $ convexDisc discs
["D","B","A","C","E","D"]
ghci> drawHull discs
Just
[((4.446072490960814,-2.3028820165612456),(-0.3143649727032963,-1.8528415278900492)),
((-1.766735271848556,0.10924423487396981),(-1.4231328419940952,1.7860485189379272)),
((-0.8286180698478034,2.5646430505529287),(2.0979311692294997,4.046259655052236)),
((2.215832152959494,4.100061219207651),(4.80833331191439,5.15833220376484)),
((8.115788737972293,1.3034266926169231),(6.6093941316249945,-1.208872328180471))]
-}
-- | Un pandero
{-
discA = Disc { did = "A", center = (0.1,2.45), radius = 3.7120344825985}
discB = Disc { did = "B", center = (0,-0.81), radius = 0.8121576201699}
discC = Disc { did = "C", center = (-3.38,2.61), radius = 0.6356099432828}
discD = Disc { did = "D", center = (0.12,5.83), radius = 0.6412487816753}
discE = Disc { did = "E", center = (3.9322728737072,2.4715986756409), radius = 0.5727128425311}
discs = [discA, discB, discC, discD, discE]
-}
{-
ghci> map did $ convexDisc discs
["B","A","C","A","D","A","B"]
ghci> drawHull discs
Just
[((-0.39367727980003553,-1.520364835397474),(-1.6993350075661453,-0.7967820269533421)),
((-3.2546191698685623,0.8607453869342807),(-3.9544098850081117,2.3378726695002285)),
((-3.9270161159444203,2.93368714663552),(-3.09463643761681,4.340369813409876)),
((-1.4312165916212596,5.831504953351996),(-0.1445155314319156,6.4141502663111165)),
((0.39140978809987637,6.4109791105742415),(1.6711257800901953,5.813147897898082)),
((3.1287289969083947,4.596159514874555),(4.399561634414381,2.8027198228314707)),
((4.403264225563158,2.1457656716696176),(3.1527273168316614,0.3381155502561044)),
((1.6969502044409202,-0.9009625549290186),(0.34939742172348137,-1.5431585378981953))]
-}
-- | Muchos círculos
{-
discA = Disc { did = "A", center = (-7.9619869423256,8.8024143841744), radius = 1.4202278465776634}
discB = Disc { did = "B", center = (-4.4768409209611,11.5201888045045), radius = 3.8251163061810547}
discC = Disc { did = "C", center = (-5.851715039481,5.31726836281), radius = 6.766448337880996}
discD = Disc { did = "D", center = (-3.7094693199268,6.5002995810713), radius = 2.8175083452396055}
discE = Disc { did = "E", center = (-10,6), radius = 3.157116798935396}
discF = Disc { did = "F", center = (3.2927965395119,11.1684768206971), radius = 5.854265264836726}
discG = Disc { did = "G", center = (-9.8804159449115,14.7815181089006), radius = 5.314294579865679}
discH = Disc { did = "H", center = (0.2552839520842,6.6281948479104), radius = 3.1652463662247}
discI = Disc { did = "I", center = (-13.3655619662759,9.2820216348209), radius = 3.575496267953}
discJ = Disc { did = "J", center = (-2.1107784844385,16.9237638284548), radius = 3.2318870749346}
discs = [discA,discB,discC,discD,discE,discF,discG,discH,discI,discJ]
-}
{-
ghci> map did $ convexDisc discs
["C","I","G","J","F","C"]
ghci> drawHull discs
Just
[((-11.02598854039956,0.9570256118952676),(-16.09972835136887,6.978001725884896)),
((-16.787121899989877,10.319856487843781),(-14.965913466862986,16.324062058295567)),
((-9.921300861565578,20.095655414727382),(-2.1356426374711,20.155555257217383)),
((-0.6232604796900323,19.792977004689437),(5.987298167850634,16.36579177805284)),
((6.851249263160311,6.519838740072514),(-1.7388015604860545,-5.56978406491444e-2))]
-}
