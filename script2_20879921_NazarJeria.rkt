#lang racket
(require "property_20879921_NazarJeria.rkt")
(require "game_20879921_NazarJeria.rkt")

; Crear propiedad y juego 
(define propiedad1 (propiedad 1 "Propiedad A" 200 50 #f 0 #f #f))
(define jugador1 (jugador 1 "Jugador 1" 1500 (list propiedad1) 0 #f 0))
(define juego-inicial (juego (list jugador1) '() 2000 2 0 10 5 4 "preparacion"))

; Prueba de construcción de casa 
(define propiedad-con-casa (propiedad-construir-casa propiedad1 juego-inicial))
(displayln (propiedad-casas propiedad-con-casa))  ; Debe mostrar 1

; Prueba de construcción de hotel 
(define propiedad-con-hotel (propiedad-construir-hotel propiedad-con-casa juego-inicial))
(displayln (propiedad-esHotel propiedad-con-hotel))  ; Debe mostrar #t

