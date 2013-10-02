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
usarT (TicketUsado t1) 	  = (TicketUsado t1)

peliculaMenosVistaT:: [Ticket] -> Pelicula
peliculaMenosVistaT ts = auxMenosVista (peliculasTs ts) ts

auxMenosVista:: [Pelicula] -> [Ticket] -> Pelicula
auxMenosVista [p] _ = p
auxMenosVista (a:b:ps) ts 	| (aparicionesPelicula a (peliculasTs (ticketsUsados ts))) 
								< (aparicionesPelicula b (peliculasTs (ticketsUsados ts))) = a
							| otherwise = auxMenosVista (b:ps) ts

peliculasTs::[Ticket]->[Pelicula]
peliculasTs [] = []
peliculasTs (t:ts) = (peliculaT t):(peliculasTs ts)

ticketsUsados:: [Ticket] -> [Ticket]
ticketsUsados [] = []
ticketsUsados ((TicketUsado t):ts) = (TicketUsado t):(ticketsUsados ts)
ticketsUsados ((TicketSinUsar _ _):ts) = ticketsUsados ts

aparicionesPelicula::Pelicula->[Pelicula]->Int
aparicionesPelicula peli [] = 0
aparicionesPelicula peli (p:ps) | p == peli = 1 + aparicionesPelicula peli ps
					  		  	| otherwise = aparicionesPelicula peli ps


todosLosTicketsParaLaMismaSalaT::[Ticket]->Bool
todosLosTicketsParaLaMismaSalaT [] = True
todosLosTicketsParaLaMismaSalaT [t] = True
todosLosTicketsParaLaMismaSalaT (x:y:ts) = salaT x == salaT y && todosLosTicketsParaLaMismaSalaT ts

cambiarSalaT::[Ticket]->Int->Int->[Ticket]
cambiarSalaT [] vieja nueva = []
cambiarSalaT (t:ts) vieja nueva | salaT t == vieja && usadoT t = (TicketUsado (TicketSinUsar nueva (peliculaT t))):(cambiarSalaT ts vieja nueva)
						   | salaT t == vieja && (not (usadoT t)) = (TicketSinUsar nueva (peliculaT t)):(cambiarSalaT ts vieja nueva)
						   | otherwise = t:(cambiarSalaT ts vieja nueva)
						   
