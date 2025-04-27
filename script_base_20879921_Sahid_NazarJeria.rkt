#lang racket
(require "player_20879921_NazarJeria.rkt")
(require "property_20879921_NazarJeria.rkt")
(require "board_20879921_NazarJeria.rkt")
(require "game_20879921_NazarJeria.rkt")
(require "card_20879921_NazarJeria.rkt")
(require "lab1_20879921_NazarJeria.rkt")


; Crear jugadores
(define p1 (jugador 1 "Carlos" 1500 '() 0 #f 0))
(define p2 (jugador 2 "Ana" 1500 '() 0 #f 0))
(define p3 (jugador 3 "Luis" 1500 '() 0 #f 0))

; Crear propiedades
(define prop1 (propiedad 1 "Paseo Mediterráneo" 60 2 #f 0 #f #f))
(define prop2 (propiedad 2 "Avenida Báltica" 60 4 #f 0 #f #f))

; Crear cartas
(define chance1 (carta 1 "suerte" "Avance hasta la casilla de salida" 'go-to-start))

; Crear tablero
(define empty-board (tablero '() '() '() '()))
(define board1 (tablero-agregar-propiedades empty-board (list (cons prop1 1) (cons prop2 3))))

; Crear juego
(define g0 (juego '() board1 20000 2 0 10 4 1 "preparacion"))

; Agregar jugadores al juego
(define g1 (juego-agregar-jugador g0 p1))
(define g2 (juego-agregar-jugador g1 p2))
(define g3 (juego-agregar-jugador g2 p3))

; Realizar lanzamiento de dados
(define dados-carlos (juego-lanzar-dados))

; Mover jugador
(define p1-movido (jugador-mover p1 dados-carlos g3))

; Mostrar estado
(displayln (jugador-nombre p1-movido))
(displayln (jugador-posicion p1-movido))

