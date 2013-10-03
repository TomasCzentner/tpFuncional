--Hay algunos casos "base" que fueron agregados, porque los tests se quejaban de su nula existencia

module Cine (nuevoC, nombreC, peliculasC, salasC, espectadoresC, salaC, ticketsVendidosC,  abrirSalaC, agregarPeliculaC,
			 cerrarSalaC, cerrarSalasC, cerrarSalasDeLaCadenaC, peliculaC, venderTicketC, ingresarASalaC, pasarA3DUnaPeliculaC) where

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
peliculasC (SalaConPelicula c _ p _ ) = ((peliculasC c)++[p])

salasC:: Cine -> [Sala]
salasC (C x) = []
salasC (SalaSinPelicula c s) = (s: (salasC c))
salasC (SalaConPelicula c s _ _) = (s: (salasC c ))
salasC (TicketVendido c _ ) = salasC c

espectadoresC:: Cine -> Sala -> Int
espectadoresC (C n) s = 0
espectadoresC (SalaSinPelicula c s) sala	| s == sala = 0
											| otherwise = espectadoresC c sala
espectadoresC (TicketVendido c _ ) s = espectadoresC c s
espectadoresC (SalaConPelicula c s _ i ) sala
					| s == sala = i
					| otherwise = espectadoresC c sala



salaC::Cine-> Pelicula -> Sala
salaC (C n) p = 0
salaC (SalaSinPelicula c s) peli = salaC c peli
salaC (TicketVendido c t) peli = salaC c peli
salaC (SalaConPelicula c s p _ ) peli 
									| p == peli = s
									|otherwise = salaC c peli

ticketsVendidosC:: Cine -> [Ticket]
ticketsVendidosC (C x) = []
ticketsVendidosC (SalaSinPelicula c _ ) = ticketsVendidosC c
ticketsVendidosC (SalaConPelicula c _ _ _ ) = ticketsVendidosC c
ticketsVendidosC (TicketVendido c t) = (t: ticketsVendidosC c)

abrirSalaC:: Cine -> Sala -> Cine
abrirSalaC (C n ) sala = SalaSinPelicula (C n) sala
abrirSalaC (SalaSinPelicula c s) sala 
										| s == sala = error "Error"
										| otherwise = SalaSinPelicula (abrirSalaC c sala) s
abrirSalaC (SalaConPelicula c s p i ) sala 
										| s == sala = error "Error"
										| otherwise = SalaConPelicula (abrirSalaC c sala) s p i
abrirSalaC (TicketVendido c t) sala = TicketVendido (abrirSalaC c sala) t


agregarPeliculaC:: Cine -> Pelicula -> Sala -> Cine
agregarPeliculaC (C n) _ _ = (C n)
agregarPeliculaC (SalaSinPelicula c s ) p sala 
											| sala == s = (SalaConPelicula c sala p 0)
											| otherwise = SalaSinPelicula (agregarPeliculaC c p sala) s
agregarPeliculaC (SalaConPelicula c s pe i ) p sala = SalaConPelicula (agregarPeliculaC c p sala) s pe i
agregarPeliculaC (TicketVendido c t) p sala = TicketVendido (agregarPeliculaC c p sala) t


cerrarSalaC:: Cine -> Sala -> Cine
cerrarSalaC (SalaSinPelicula c s ) sala 
										| s == sala = c
										|otherwise =SalaSinPelicula (cerrarSalaC c sala ) s
cerrarSalaC (TicketVendido c t ) sala = TicketVendido( cerrarSalaC c sala) t 
cerrarSalaC (SalaConPelicula c s p i ) sala 
											| (s == sala ) = c
											|otherwise = SalaConPelicula (cerrarSalaC c sala ) s p i

----------------------------------------------------------------------
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

--Tira error
--Fijarse el caso SalaConPelicula(TicketVendido(...))
ingresarASalaC:: Cine -> Sala -> Ticket ->  (Cine, Ticket)
ingresarASalaC (C n) sala ticket = ((C n), ticket)
ingresarASalaC (SalaConPelicula c s p i) sala ticket = ((SalaConPelicula (fst(ingresarASalaC c sala ticket)) s p i), 
														nuevoT (peliculaT ticket) (salaT ticket) True )
ingresarASalaC (TicketVendido c t ) sala ticket 
												| t == ticket = ( (agregarEspectador c sala) , nuevoT (peliculaT ticket) (salaT ticket) True)
												| otherwise = (TicketVendido (fst(ingresarASalaC c sala ticket)) t, nuevoT (peliculaT ticket) (salaT ticket) True )


agregarEspectador:: Cine-> Sala -> Cine
agregarEspectador (TicketVendido c t) sala = TicketVendido (agregarEspectador c sala) t
agregarEspectador (SalaConPelicula c s p i ) sala
												| s == sala = SalaConPelicula c s p (i+1)
												| otherwise = SalaConPelicula (agregarEspectador c sala) s p i 

--Tira error
pasarA3DUnaPeliculaC :: Cine -> Nombre -> (Cine, Pelicula)
pasarA3DUnaPeliculaC (C n) nombre = ((C n), nuevaP "" [] [] True) --No importa la peli, porque sólo queremos (fst result)
pasarA3DUnaPeliculaC (TicketVendido c t ) nombre 		| nombreP(peliculaT t) == nombre = 
																	(TicketVendido (fst(pasarA3DUnaPeliculaC c  nombre)) (nuevoT (peli3D c nombre) (salaT t) True), peli3D c nombre)
														| otherwise = (TicketVendido (fst(pasarA3DUnaPeliculaC c  nombre)) t, peli3D c nombre)
pasarA3DUnaPeliculaC (SalaConPelicula c s p i ) nombre 
														| (nombreP p ) == nombre = 
																	(SalaConPelicula (fst(pasarA3DUnaPeliculaC c  nombre)) s (peli3D (SalaConPelicula c s p i) nombre) i, peli3D (SalaConPelicula c s p i) nombre  )
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
c8 = TicketVendido c7 ticket5
c9 = SalaConPelicula c8 8 peli20 30
c10 = abrirSalaC c9 3
