module Pelicula (Pelicula, nuevaP, nombreP, generosP, actoresP, es3DP, p1 {-, agruparPelisPorGeneroP, generarSagaDePeliculasP-}) where

import Tipos

data Pelicula = P Nombre [Genero] [Actor] Bool deriving (Show, Eq)

nuevaP:: Nombre -> [Genero] -> [Actor] -> Bool -> Pelicula
nuevaP n gs as b = (P n (auxGenerosSinRepetidos gs) (auxActoresSinRepetidos as) b)

nombreP:: Pelicula -> Nombre
nombreP (P n _ _ _) = n

generosP:: Pelicula -> [Genero]
generosP (P _ gs _ _) = gs

actoresP:: Pelicula -> [Actor]
actoresP (P _ _ as _) = as

es3DP:: Pelicula -> Bool
es3DP (P _ _ _ b) = b

-- generarSagaDePeliculasP:: [Actor] -> [Genero] -> [Nombre] -> [Pelicula]
-- generarSagaDePeliculasP as gs (n:ns) = (nuevaP n as gs False):generarSagaDePeliculasP as gs ns
-- ganerarSagaDePeliculasP [] = []
-- Ver que poner con respecto a el bool 3D



auxGenerosSinRepetidos:: [Genero] -> [Genero]
auxGenerosSinRepetidos [] = []
auxGenerosSinRepetidos (x:xs) 	| (elem x xs) = auxGenerosSinRepetidos xs
								| otherwise = x:(auxGenerosSinRepetidos xs)

auxActoresSinRepetidos:: [Actor] -> [Actor]
auxActoresSinRepetidos [] = []
auxActoresSinRepetidos (x:xs) 	| (elem x xs) = auxActoresSinRepetidos xs
								| otherwise = x:(auxActoresSinRepetidos xs)

n1 = "LOTR I"
n2 = "LOTR II"
n3 = "LOTR III"
p1 = nuevaP n1 [Aventura] as1 False
as1 = ["Orlando Bloom","Vigo Mortensein","Ian McKellen"]							
as = ["Robert", "Soy", "No", "Soy", "Hola", "Pedro", "Soy"]
gs = [Aventura, Comedia, Drama, Aventura, Comedia, Comedia]
