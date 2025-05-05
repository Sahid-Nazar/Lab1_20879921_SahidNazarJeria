#lang racket

(require "player_20879921_NazarJeria.rkt")
(require "property_20879921_NazarJeria.rkt")
(require "card_20879921_NazarJeria.rkt")
(require "board_20879921_NazarJeria.rkt")
(require "game_20879921_NazarJeria.rkt")

(provide
  ; Player
  jugador jugador-id jugador-nombre jugador-dinero jugador-propiedades jugador-posicion
  jugador-estaEnCarcel jugador-totalCartasSalirCarcel jugador-mover jugador-comprar-propiedad
  jugador-pagar-renta jugador-esta-en-bancarrota 

  ; Property
  propiedad propiedad-id propiedad-nombre propiedad-precio propiedad-renta propiedad-dueño
  propiedad-casas propiedad-esHotel propiedad-estaHipotecada propiedad-calcular-renta
  propiedad-hipotecar propiedad-construir-casa propiedad-construir-hotel propiedad-actualizar-dueno

  ; Card
  carta

  ; Board
  tablero tablero-agregar-propiedad tablero-obtener-propiedad tablero-propiedades
  tablero-cartas-suerte tablero-cartas-comunidad tablero-casillas-especiales
  tablero-es-casilla-especial?

  ; Game
  juego lanzar-dados juego-agregar-jugador juego-obtener-jugador-actual juego-lista-jugadores
  juego-tablero juego-dineroBanco juego-numeroDados juego-indiceTurnoActual juego-tasaImpuesto
  juego-maximoCasas juego-maximoHoteles juego-actualizar-jugador juego-actualizar-propiedad
  juego-jugar-turno jugador-construir-casa jugador-construir-hotel juego-manejar-carcel get-jugador)
