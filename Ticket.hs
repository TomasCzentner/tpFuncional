module Ticket (Ticket, nuevoT{-, salaT, peliculaT, usadoT, usarT, peliculaMenosVistaT, todosLosTicketsParaLaMismaSalaT, cambiarSalaT-}) 	where

import Tipos
import Pelicula

data Ticket = TicketSinUsar Sala Pelicula | TicketUsado Ticket deriving (Show, Eq)

nuevoT::Pelicula->Sala->Bool->Ticket
nuevoT p s b | b == False 	= TicketSinUsar s p
			 | otherwise 	= TicketUsado (nuevoT p s (not b))

--peliculaT::Ticket->

