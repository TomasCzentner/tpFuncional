module Cine (nuevoC, nombreC, peliculasC, {-salasC, espectadores, salaC, ticketsVendidosC,  abrirSalaC, agregarPeliculaC,
			 cerrarSalaC-} cerrarSalasC, cerrarSalasDeLaCadenaC, peliculaC, venderTicketC, ingresarASalaC, pasarA3DUnaPeliculaC) where

import Tipos
import Pelicula
import Ticket

data Cine = C Nombre | 
			SalaSinPelicula Cine Sala | 
			SalaConPelicula Cine Sala Pelicula Int | 
			TicketVendido Cine Ticket deriving (Show)

nuevoC:: Nombre -> Cine
nuevoC n = (C n)

nombreC:: Cine -> Nombre 
nombreC (C x) = x 
nombreC (SalaSinPelicula c _ ) = nombreC c 
nombreC (SalaConPelicula c _ _ _ ) = nombreC c 
nombreC (TicketVendido c _ ) = nombreC c

peliculasC:: Cine -> [Pelicula] 
peliculasC (C x) = [] 
peliculasC (SalaSinPelicula c _ ) = peliculasC c 
peliculasC (TicketVendido c _ ) = peliculasC c 
peliculasC (SalaConPelicula c _ p _ ) = (p: (peliculasC c))

--espectadores

salaC::Cine-> Pelicula -> Sala
salaC (TicketVendido c t) peli = salaC c peli
salaC (SalaConPelicula c s p _ ) peli 
									| p == peli = s
									|otherwise = salaC c peli

cerrarSalasC:: Cine -> Int -> Cine
cerrarSalasC (C n) _ = C n
cerrarSalasC (TicketVendido c t ) e = TicketVendido (cerrarSalasC c e) t 
cerrarSalasC (SalaSinPelicula c s ) e = cerrarSalasC c e 
cerrarSalasC (SalaConPelicula c s p i ) e 
											| i < e =  cerrarSalasC c e 
											| otherwise = SalaConPelicula (cerrarSalasC c e) s p i

cerrarSalasDeLaCadenaC:: [Cine] -> Int -> [Cine]
cerrarSalasDeLaCadenaC [] e = []
cerrarSalasDeLaCadenaC (c:cs) e = ((cerrarSalasC c e): (cerrarSalasDeLaCadenaC cs e )) 

peliculaC:: Cine -> Sala -> Pelicula
peliculaC (TicketVendido c t ) sala = peliculaC c sala 
peliculaC (SalaConPelicula c s p i ) sala 
											| s == sala = p
											|otherwise = peliculaC c sala 

venderTicketC :: Cine -> Pelicula -> (Cine, Ticket)
venderTicketC c peli = (TicketVendido c (nuevoT peli (salaC c peli) False ) , nuevoT peli (salaC c peli) False )


ingresarASalaC:: Cine -> Sala -> Ticket ->  (Cine, Ticket)

ingresarASalaC (TicketVendido c t ) sala ticket 
												| t == ticket = (( agregarEspectador c sala ticket) , nuevoT (peliculaT t) (salaT t) True)
												| otherwise = (TicketVendido (fst(ingresarASalaC c sala ticket)) t, nuevoT (peliculaT ticket) (salaT ticket) True )


agregarEspectador:: Cine-> Sala -> Ticket -> Cine
agregarEspectador (TicketVendido c t) sala ticket = TicketVendido (agregarEspectador c sala ticket) t
agregarEspectador (SalaConPelicula c s p i ) sala ticket 
														| s== sala = SalaConPelicula c s p (i+1)
														|otherwise = SalaConPelicula (agregarEspectador c sala ticket) s p i 

--fijarse duplas
pasarA3DUnaPeliculaC :: Cine -> Nombre -> (Cine, Pelicula)
pasarA3DUnaPeliculaC (TicketVendido c t ) nombre 		| nombreP(peliculaT t) == nombre = 
																	(TicketVendido (fst(pasarA3DUnaPeliculaC c  nombre)) (nuevoT (peli3D c nombre) (salaT t) True), peli3D c nombre)
														| otherwise = (TicketVendido (fst(pasarA3DUnaPeliculaC c  nombre)) t, peli3D c nombre)
pasarA3DUnaPeliculaC (SalaConPelicula c s p i ) nombre 
														| (nombreP p ) == nombre = 
																	(SalaConPelicula c s (peli3D (SalaConPelicula c s p i) nombre) i, peli3D (SalaConPelicula c s p i) nombre  )
														|otherwise = ((SalaConPelicula (fst(pasarA3DUnaPeliculaC c nombre)) s p i) , peli3D (SalaConPelicula c s p i) nombre)

peli3D:: Cine -> Nombre -> Pelicula
peli3D (TicketVendido c t ) nombre = peli3D c nombre
peli3D (SalaConPelicula c s p i)  nombre 
										| nombreP p == nombre = nuevaP nombre (generosP p) (actoresP p ) True 
										| otherwise = peli3D c nombre

ticket4 = nuevoT peli33 4 False
ticket5 = nuevoT peli20 3 False
peli20 = nuevaP "La cosa" [Terror] ["Pedro"] False
peli33 = nuevaP "Hey" [Aventura] ["Jorge"] False
c1 = nuevoC "cine1"
c2 = nuevoC "cine2"
c3 = SalaSinPelicula c1 8
c4 = SalaConPelicula c2 9 peli33 10
c5 = TicketVendido c4 ticket4
c6 = SalaConPelicula c2 7 peli20 80
c7 = TicketVendido c6 ticket5