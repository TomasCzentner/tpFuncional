module Pelicula (Pelicula, nuevaP{-, nombreP, generosP, actoresP, es3DP, agruparPelisPorGeneroP, generarSagaDePeliculasP-}) where

import Tipos

data Pelicula = P Nombre [Genero] [Actor] Bool deriving (Show, Eq)

nuevaP:: Nombre -> [Genero] -> [Actor] -> Bool -> Pelicula
nuevaP n g a b = (P n (auxGenerosSinRepetidos g) (auxActoresSinRepetidos a) b)

nombreP:: Pelicula -> Nombre
nombreP (P n _ _ _) = n


auxGenerosSinRepetidos:: [Genero] -> [Genero]
auxGenerosSinRepetidos [] = []
auxGenerosSinRepetidos (x:xs) 	| (elem x xs) = auxGenerosSinRepetidos xs
								| otherwise = x:(auxGenerosSinRepetidos xs)

auxActoresSinRepetidos:: [Actor] -> [Actor]
auxActoresSinRepetidos [] = []
auxActoresSinRepetidos (x:xs) 	| (elem x xs) = auxActoresSinRepetidos xs
								| otherwise = x:(auxActoresSinRepetidos xs)
		
													
a = ["Robert", "Soy", "No", "Soy", "Hola", "Pedro", "Soy"]
gs = [Aventura, Comedia, Drama, Aventura, Comedia, Comedia]