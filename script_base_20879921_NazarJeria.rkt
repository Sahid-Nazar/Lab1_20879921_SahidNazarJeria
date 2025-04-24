#lang racket
(require "player_20879921_NazarJeria.rkt")
(require "property_20879921_NazarJeria.rkt")
(require "board_20879921_NazarJeria.rkt")
(require "game_20879921_NazarJeria.rkt")

; Prueba de creación de jugador 
(define jugador1 (jugador 1 "Jugador 1" 1500 '() 0 #f 0))
(define jugador2 (jugador 2 "Jugador 2" 1500 '() 0 #f 0))

; Imprimir jugadores para verificar la creación
(displayln (jugador-nombre jugador1))  ; Debe mostrar "Jugador 1"
(displayln (jugador-dinero jugador2))  ; Debe mostrar 1500

; Prueba de creación de propiedad 
(define propiedad1 (propiedad 1 "Propiedad A" 200 50 #f 0 #f #f))

; Verificar los valores de la propiedad
(displayln (propiedad-nombre propiedad1))  ; Debe mostrar "Propiedad A"
(displayln (propiedad-precio propiedad1))  ; Debe mostrar 200

; Crear Tablero 
(define tablero-inicial (tablero '() '() '() '()))

; Agregar propiedad al tablero
(define tablero-actualizado (tablero-agregar-propiedades tablero-inicial (list (cons propiedad1 1))))

; Verificar que se ha agregado la propiedad
(displayln (length (tablero-propiedades tablero-actualizado)))  ; Debe mostrar 1

; Crear juego 
(define juego-inicial (juego (list jugador1 jugador2) tablero-inicial 2000 2 0 10 5 4 "preparacion"))

; Verificar el estado del juego
(displayln (juego-estadoJuego juego-inicial))  ; Debe mostrar "preparacion"
