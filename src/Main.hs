-- | Módulo main en donde se ejecutará el programa para generar el
-- cierre convexo.
module Main where

import System.Environment

import CircleReader

-- | Función main, obtiene los argumentos con los que fue llamado el
-- programa, los cuales deben ser el archivo que se leerá y el archivo
-- en donde se escribirá el resultado
main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output] -> do
      discs <- genCircles input
      writeHull output discs
      putStrLn "¡Discs generated!"
    _ -> putStrLn "Wrong number of arguments"
