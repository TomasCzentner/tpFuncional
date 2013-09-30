import Tipos
import Pelicula

data Cine = C Nombre | 
			SalaSinPelicula Cine Sala | 
			SalaConPelicula Cine Sala Pelicula Int | 
			TicketVendido Cine Ticket deriving (Show)



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


data Ticket = TicketSinUsar Sala Pelicula | TicketUsado Ticket deriving (Show, Eq)

nuevoT::Pelicula->Sala->Bool->Ticket
nuevoT p s b | b == False 	= TicketSinUsar s p
			 | otherwise 	= TicketUsado (nuevoT p s (not b))

--peliculaT::Ticket->




--agruparPelisPorGeneroP:: [Pelicula] -> [(Genero, [Pelicula])]
--agruparPelisPorGeneroP [] = []
--agruparPelisPorGeneroP (x:xs) = ((generosP x), auxPelisDeGenero 

--auxPelisDeGenero:: [Pelicula] -> Genero -> [Pelicula]
--auxPelisDeGenero [] _ = []
--auxPelisDeGenero (x:xs) g 
--							| (elem g (generosP (x)) = x:(auxPelisDeGenero xs g)
--							| otherwise = auxPelisDeGenero xs g

--auxDuplas:: Pelicula -> [(Genero, Pelicula)]

--auxDuplas 

auxGenerosDePelis:: [Pelicula] -> [Genero]
auxGenerosDePelis [] = []
auxGenerosDePelis (x:xs) = (generosP x) ++ (auxGenerosDePelis xs)

generarSagaDePeliculasP:: [Actor] -> [Genero] -> [Nombre] -> [Pelicula]
generarSagaDePeliculasP _ _ [] = []
generarSagaDePeliculasP as gs (x:xs) = 
				((P x (auxGenerosSinRepetidos gs) (auxActoresSinRepetir a)True) : generarSagaDePeliculasP as gs xs) 
