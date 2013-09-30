module Ticket (Ticket, nuevoT, salaT, peliculaT, usadoT, usarT, peliculaMenosVistaT, todosLosTicketsParaLaMismaSalaT, cambiarSalaT) 	where

import Tipos
import Pelicula

data Ticket = TicketSinUsar Sala Pelicula | TicketUsado Ticket deriving (Show, Eq)

nuevoT::Pelicula->Sala->Bool->Ticket
nuevoT p s b | b == False 	= TicketSinUsar s p
			 | otherwise 	= TicketUsado (nuevoT p s (not b))

peliculaT::Ticket->Pelicula
peliculaT (TicketSinUsar s p) = p
peliculaT (TicketUsado (TicketSinUsar s p)) = p

salaT::Ticket->Sala
salaT (TicketSinUsar s p) = s
salaT (TicketUsado (TicketSinUsar s p)) = s

usadoT::Ticket->Bool
usadoT (TicketSinUsar s p) = False
usadoT (TicketUsado (TicketSinUsar s p)) = True

usarT::Ticket->Ticket
usarT (TicketSinUsar s p) = (TicketUsado (TicketSinUsar s p))
usarT (TicketUsado t1) 	  = t1

peliculaMenosVistaT::[Ticket]->Pelicula
peliculaMenosVistaT [t1] = peliculaT t1
peliculaMenosVistaT [t1,t2] = peliculaT t1
peliculaMenosVistaT (x:y:ts) | aparicionesPelicula (peliculaT x) (peliculasTs (x:y:ts)) <= aparicionesPelicula (peliculaT y) (peliculasTs (y:ts)) = peliculaT x
			   			   	 | otherwise = peliculaMenosVistaT (y:ts)

peliculasTs::[Ticket]->[Pelicula]
peliculasTs [t1] = [(peliculaT t1)]
peliculasTs (t:ts) = (peliculaT t):(peliculasTs ts)

aparicionesPelicula::Pelicula->[Pelicula]->Int
aparicionesPelicula p1 [] = 0
aparicionesPelicula p1 (p:ps) | p1 == p = 1 + aparicionesPelicula p1 ps
					  		  | otherwise = aparicionesPelicula p1 ps

todosLosTicketsParaLaMismaSalaT::[Ticket]->Bool
todosLosTicketsParaLaMismaSalaT [] = True
todosLosTicketsParaLaMismaSalaT [t] = True
todosLosTicketsParaLaMismaSalaT (x:y:ts) = salaT x == salaT y && todosLosTicketsParaLaMismaSalaT ts

cambiarSalaT::[Ticket]->Int->Int->[Ticket]
cambiarSalaT [] vieja nueva = []
cambiarSalaT (t:ts) vieja nueva | salaT t == vieja && usadoT t = (TicketUsado (TicketSinUsar nueva (peliculaT t))):(cambiarSalaT ts vieja nueva)
						   | salaT t == vieja && (not (usadoT t)) = (TicketSinUsar nueva (peliculaT t)):(cambiarSalaT ts vieja nueva)
						   | otherwise = t:(cambiarSalaT ts vieja nueva)