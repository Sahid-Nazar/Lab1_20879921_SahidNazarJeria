#lang racket
(require "property_20879921_NazarJeria.rkt")
(require "player_20879921_NazarJeria.rkt")

; Crear jugadores y propiedades 
(define jugador1 (jugador 1 "Jugador 1" 1500 '() 0 #f 0))
(define propiedad1 (propiedad 1 "Propiedad A" 200 50 #f 0 #f #f))

; Prueba de compra de propiedad 
(define jugador-actualizado (jugador-comprar-propiedad jugador1 propiedad1))

; Verificar que el jugador compró la propiedad
(displayln (length (jugador-propiedades jugador-actualizado)))  ; Debe mostrar 1

; Prueba de cálculo de renta 
(displayln (propiedad-calcular-renta jugador1 propiedad1))  ; Debe mostrar 50 (renta base)
