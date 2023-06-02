-- | Módulo de algunas primitivas geométricas
module Primitives where

-- | Sinónimo para puntos, descritos por una tupla correspondiente al
-- valor en el eje x, y al eje y respectivamente.
type Point = (Double, Double)

-- | Tipo de dato para describir el sentido que toman tres puntos
-- dados.
data Orientation = Col
                 | Ccw
                 | Cw deriving (Show, Eq)

-- | Función que dados tres puntos, dos dice hacia qué dirección se
-- encuentra el tercero con respecto a los primeros dos.
orientation :: Point -> Point -> Point -> Orientation
orientation p q r
  | value == 0 = Col
  | value > 0 = Ccw
  | otherwise = Cw
  where value = (snd r - snd p) * (fst q - fst p) - (snd q - snd p) * (fst r - fst p)

-- | Sinónimo para un segmento de recta dirigido, descrita por una
-- tupla con dos puntos.
type Line = (Point, Point)

-- | Función que dada una línea y un punto, nos dice hacia qué
-- dirección se encuentra el tercero con respecto a la línea.
-- La función uncurry envuelve en una tupla los primeros dos
-- parámetros de una función.
lineOrientation :: Line -> Point -> Orientation
lineOrientation = uncurry orientation

-- | Función que dada una línea, nos regresa su longitud.
lineDistance :: Line -> Double
lineDistance ((x1,y1),(x2,y2)) = sqrt $ (x2-x1)^2 + (y2-y1)^2

-- | Tipo de dato para definir lineas dirigidas.
-- Guarda un punto por donde pasa la línea y el vector director.
data VectDir = VectDir { p :: Point
                       , dx :: Double
                       , dy :: Double
                       } deriving Show

-- | Función que transforma un segmento de recta en una línea dirigida
lineToDir :: Line -> VectDir
lineToDir ((x1,y1),(x2,y2)) = VectDir { p = (x1,y1), dx = x2-x1, dy = y2 - y1}

-- | Función que transforma una línea dirigida en un segmento de
-- recta.
dirToLine :: VectDir -> Line
dirToLine vect = (p vect, p2)
  where
    (x,y) = p vect
    p2 = (x + dx vect, y + dy vect)

-- | Función que normaliza la dirección de una línea dirigida.
normalize :: VectDir -> VectDir
normalize vect = VectDir { p = p vect,  dx = dx vect / norm, dy = dy vect / norm }
  where
    norm = sqrt $ (dx vect)^2 + (dy vect)^2

-- | Función que dadas dos líneas dirigidas l1 y l2, nos regresa el
-- ángulo desde l1 a l2 hacia la derecha.
dirAngle :: VectDir -> VectDir -> Double
dirAngle v1 v2
  | sameDir v1 v2 = 0
  | orientation (0,0) (dx v1, dy v1) (dx v2, dy v2) == Ccw = 2 * pi - angle
  | otherwise = angle
  where
    angle = acos (pointProd / (normV1 * normV2))
    pointProd = dx v1 * dx v2 + dy v1 * dy v2
    normV1 = sqrt $ (dx v1)^2 + (dy v1)^2
    normV2 = sqrt $ (dx v2)^2 + (dy v2)^2

-- | Función que dadas dos línea dirigidas, nos regresa si apuntan
-- exactamente a la misma dirección.
sameDir :: VectDir -> VectDir -> Bool
sameDir v1 v2 = (dx normalV1, dy normalV1) == (dx normalV2, dy normalV2)
  where
    normalV1 = normalize v1
    normalV2 = normalize v2

-- | Tipo de dato para definir discos definidos por un identificador,
-- su centro y su radio.
data Disc = Disc { did :: String
                 , center :: Point
                 , radius :: Double} deriving (Show, Eq)

-- | Función que dados dos discos, nos regresa las líneas exteriores
-- (si es que existen).
outterTangents :: Disc -> Disc -> Maybe (Line, Line)
outterTangents discA discB
  | radius discA < radius discB = outterTangents discB discA
  | radius discA >= hyp + radius discB = Nothing
  | radius discB >= hyp + radius discA = Nothing
  | otherwise = Just (line1, line2)
  where
    hyp = lineDistance (center discA, center discB)
    r = abs $ radius discA - radius discB
    phi1 = atan2 (snd (center discB) - snd (center discA)) (fst (center discB) - fst (center discA)) + acos (r / hyp)
    t1x = fst (center discA) + radius discA * cos phi1
    t1y = snd (center discA) + radius discA * sin phi1
    p1 = (t1x, t1y)
    t2x = fst (center discB) + radius discB * cos phi1
    t2y = snd (center discB) + radius discB * sin phi1
    p2 = (t2x, t2y)
    line1 = (p1, p2)
    phi2 = atan2 (snd (center discB) - snd (center discA)) (fst (center discB) - fst (center discA)) - acos (r / hyp)
    t3x = fst (center discA) + radius discA * cos phi2
    t3y = snd (center discA) + radius discA * sin phi2
    p3 = (t3x, t3y)
    t4x = fst (center discB) + radius discB * cos phi2
    t4y = snd (center discB) + radius discB * sin phi2
    p4 = (t4x, t4y)
    line2 = (p3,p4)

-- | Función que dados dos discos A y B, calcula las tangentes de A y
-- B (si es que existen) y nos regresa aquella que con respecto a los
-- centros de A y B se encuentra en sentido antihorario.
tangentFromDiscToDisc :: Disc -> Disc -> Maybe Line
tangentFromDiscToDisc discA discB = do
      (l1,l2) <- outterTangents discA discB
      if radius discA < radius discB then do
        let fun (x,y) = (y,x)
        let (fl1, fl2) = (fun l1, fun l2)
        case lineOrientation fl2 (snd fl1) of
          Cw -> return fl2
          _ -> return fl1
        else case lineOrientation l2 (snd l1) of
               Cw -> return l2
               _ -> return l1

-- | Función que dada una línea dirigida L y un círculo D, nos regresa
-- la línea dirigida perpendicular a L que pasa desde el centro de D,
-- hacia su intersección con L.
perpFromCircle :: VectDir -> Disc -> VectDir
perpFromCircle vect disc
  | dx vect == 0 = VectDir {p = center disc, dx = if x1 < cx then -(radius disc) else radius disc, dy = 0}
  | dy vect == 0 = VectDir {p = center disc, dx = 0, dy = if y1 < cy then -(radius disc) else radius disc}
  | otherwise = VectDir {p = center disc, dx = x - cx, dy = y - cy}
  where
    (x1, y1) = p vect
    (cx, cy) = center disc
    x = (cy - y1 + m*x1 + m'*cx) / (m + m')
    m = (dy vect / dx vect)
    m' = (dx vect / dy vect)
    y = m * (x-x1) + y1

-- | Función que dada una línea dirigida L y un círculo D, regresa la
-- linea dirigida paralela a L y tangente a D que se encuentra más a
-- la izquierda con respecto a L.
paraFromCircle :: VectDir -> Disc -> VectDir
paraFromCircle vect disc = (normalize vect) { p = point }
  where
    norm = normalize $ perpFromCircle vect disc
    (x, y) = p norm
    r = radius disc
    point = if lineOrientation (dirToLine vect) (center disc) == Cw then
              (x + r * dx norm, y + r * dy norm)
            else
              (x - r * dx norm, y - r * dy norm)

-- | Función que dadas dos líneas dirigidas Lp y Lq paralelas, nos
-- dice si la mitad de plano derecha de Lq está contenida dentro de la
-- mitad de plano derecha de Lp.
dom :: VectDir -> VectDir -> Bool
dom = ((Ccw /=) .) . (. p) . lineOrientation . dirToLine

data InfList a = Inf [a]

inf :: [a] -> [a]
inf [] = []
inf xs = xs ++ inf xs

mkInf :: [a] -> InfList a
mkInf = Inf . inf

next :: InfList a -> InfList a
next (Inf []) = Inf []
next (Inf (_:xs)) = Inf xs

current :: InfList a -> Maybe a
current (Inf []) = Nothing
current (Inf (x:_)) = Just x
