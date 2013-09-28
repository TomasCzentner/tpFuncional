module Pelicula (Pelicula, nuevaP, nombreP, generosP, actoresP, es3DP, agruparPelisPorGeneroP,{- generarSagaDePeliculasP-}) where

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

--Intentar hacerlo más lindo
agruparPelisPorGeneroP:: [Pelicula] -> [(Genero, [Pelicula])]
agruparPelisPorGeneroP ps = agruparPelisPorGenero2 ps (auxGenerosSinRepetidos (auxGenerosDePelis ps))


agruparPelisPorGenero2:: [Pelicula] -> [Genero] ->[(Genero, [Pelicula])]
agruparPelisPorGenero2 _ [] = []
agruparPelisPorGenero2 ps (g:gs) = (g, (auxPelisDeGenero ps g)):(agruparPelisPorGenero2 ps gs)

auxGenerosDePelis:: [Pelicula] -> [Genero]
auxGenerosDePelis [] = []
auxGenerosDePelis (x:xs) = (generosP x) ++ (auxGenerosDePelis xs)

auxPelisDeGenero:: [Pelicula] -> Genero -> [Pelicula]
auxPelisDeGenero [] _ = []
auxPelisDeGenero (x:xs) g 	| (elem g (generosP x)) = x: (auxPelisDeGenero xs g)
							| otherwise = auxPelisDeGenero xs g


auxGenerosSinRepetidos:: [Genero] -> [Genero]
auxGenerosSinRepetidos [] = []
auxGenerosSinRepetidos (x:xs) 	| (elem x xs) = auxGenerosSinRepetidos xs
								| otherwise = x:(auxGenerosSinRepetidos xs)

auxActoresSinRepetidos:: [Actor] -> [Actor]
auxActoresSinRepetidos [] = []
auxActoresSinRepetidos (x:xs) 	| (elem x xs) = auxActoresSinRepetidos xs
								| otherwise = x:(auxActoresSinRepetidos xs)
		
										
tst_acts = ["Robert", "Jamie", "Leonard", "Jamie", "Robert", "Pedro", "Jane"]
tst_acts2 = ["Hey"]
tst_grs = [Aventura, Comedia, Drama]
tst_grs2 = [Terror, Comedia]
tst_grs3= [Drama, Aventura]
tst_peli = nuevaP "Nuevap" tst_grs tst_acts2 True
tst_peli2 = nuevaP "ADSA" tst_grs2 tst_acts2 False
tst_peli3 = nuevaP "La pelicula" tst_grs3 tst_acts2 False
tst_peli4 = nuevaP "La peli2" [] tst_acts2 False
listadepelis = [tst_peli, tst_peli4, tst_peli3]
