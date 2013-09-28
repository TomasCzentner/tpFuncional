module Ticket (Ticket, nuevoT, salaT, peliculaT, usadoT, usarT{-, peliculaMenosVistaT, todosLosTicketsParaLaMismaSalaT, cambiarSalaT-}) 	where

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

{-peliculaMenosVistaT::[Ticket]->Pelicula
peliculaMenosVista [(TicketSinUsar s p)] = p 
peliculaMenosVistaT ((TicketSinUsar s p):ts) = 
peliculaMenosVistaT ((TicketUsado(TicketSinUsar(s p)))):ts) =

apariciones::Char->[Char]->Int
apariciones a (b:as) | a == b = 1 + apariciones a as
					 | otherwise = apariciones a as
apariciones a [] = 0

aparicionesPelis::Pelicula->[Ticket]->Int
aparicionesPelis p ()-}