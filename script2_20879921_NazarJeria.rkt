#lang racket
(require "property_20879921_NazarJeria.rkt")
(require "game_20879921_NazarJeria.rkt")
(require "player_20879921_NazarJeria.rkt")

; Crear propiedad y juego 
(define propiedad1 (propiedad 1 "Propiedad A" 200 50 #f 0 #f #f))
(define jugador1 (jugador 1 "Jugador 1" 1500 (list propiedad1) 0 #f 0))
(define juego-inicial (juego (list jugador1) '() 2000 2 0 10 5 4 "preparacion"))

; Prueba de construcción de casa 
(define propiedad-con-casa (propiedad-construir-casa propiedad1 juego-inicial))
(displayln (propiedad-casas propiedad-con-casa))  ; Debe mostrar 1

; Verificar si la propiedad está hipotecada después de la construcción de casa
(displayln (propiedad-estaHipotecada propiedad-con-casa))  ; Debe mostrar #f (si no está hipotecada)

; Construir más casas para llegar a 4 casas
(define propiedad-con-2-casas (propiedad-construir-casa propiedad-con-casa juego-inicial))
(define propiedad-con-3-casas (propiedad-construir-casa propiedad-con-2-casas juego-inicial))
(define propiedad-con-4-casas (propiedad-construir-casa propiedad-con-3-casas juego-inicial))

; Verificar si ya tiene 4 casas
(displayln (propiedad-casas propiedad-con-4-casas))  ; Debe mostrar 4

; Prueba de construcción de hotel 
(define propiedad-con-hotel (propiedad-construir-hotel propiedad-con-4-casas juego-inicial))
(displayln (propiedad-esHotel propiedad-con-hotel))  ; Debe mostrar #t
(displayln (propiedad-casas propiedad-con-hotel))    ; Debe mostrar 0 casas (porque se convirtió en hotel)
