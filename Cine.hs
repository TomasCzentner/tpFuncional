module Cine (nuevoC, nombreC, peliculasC, salasC, espectadoresC, salaC, ticketsVendidosC,  abrirSalaC, agregarPeliculaC, cerrarSalaC, cerrarSalasC, cerrarSalasDeLaCadenaC, peliculaC, venderTicketC, ingresarASalaC, pasarA3DUnaPeliculaC ) where

import Tipos
import Pelicula
import Ticket

data Cine = C Nombre | 
			SalaSinPelicula Cine Sala | 
			SalaConPelicula Cine Sala Pelicula Int | 
			TicketVendido Cine Ticket deriving (Show)

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
