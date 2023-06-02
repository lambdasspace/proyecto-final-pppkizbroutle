-- | Módulo para leer círculos y escribir su cierre convexo en
-- archivos
module CircleReader where

import Control.Arrow

import qualified Data.ByteString.Char8 as By
import Data.Char
import Data.Function
import Data.List
import Data.Maybe

import System.IO

import Primitives
import ConvexDiscs

-- | Función que dado una cadena, intenta traducir todos los círculos
-- que encuentre
parseCircles :: String -> [Disc]
parseCircles = By.pack
               >>> By.lines
               >>> map (By.split ',')
               >>> (map . map) By.unpack
               >>> map singleCircle
               >>> foldr fun []
  where
    fun Nothing acc = acc
    fun (Just x) acc = x : acc
    singleCircle :: [String] -> Maybe Disc
    singleCircle [id, px, py, r] = if isDouble px && isDouble py && isDouble r then
                                  Just $ Disc { did = id, center = (read px, read py), radius = read r}
                                else
                                     Nothing
    singleCircle _ = Nothing
    isDouble :: String -> Bool
    isDouble = (&&) . (1 >=) . length . filter (== '.') <*> all (\x -> isDigit x || (x == '-')) . filter (/= '.')

-- | Función que lee los círculos de un archivo
genCircles :: String -> IO [Disc]
genCircles = (>>= return . parseCircles) . readFile

-- | Función que dado un conjunto de líneas las convierte a una sola
-- cadena
writeLines :: [Line] -> String
writeLines = foldr
             (\((x1,y1),(x2,y2)) xs -> show x1 ++ "," ++
                                       show y1 ++ "|" ++
                                       show x2 ++ "," ++
                                       show y2 ++ "\n" ++
                                       xs) ""

-- | Función que dada una ruta y conjunto de discos, escribe el cierre
-- convexo en la ruta especificada.
writeHull :: String -> [Disc] -> IO ()
writeHull = (. writeLines . drawHull) . writeFile
