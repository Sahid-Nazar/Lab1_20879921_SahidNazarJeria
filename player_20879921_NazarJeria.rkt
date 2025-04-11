#lang racket
(provide jugador)
; Representación TDA Jugador:
; Se utiliza una lista donde cada posición representa:
; 1. (car lista)      => id (Integer)
; 2. (cadr lista)     => nombre (String)
; 3. (caddr lista)    => dinero (Integer)
; 4. (cadddr lista)   => propiedades (List) ; Lista de TDA Propiedad (inicialmente vacía)
; 5. (list-ref lista 4) => posicion (Integer) ; Posición en el tablero
; 6. (list-ref lista 5) => estaEnCarcel (Boolean)
; 7. (list-ref lista 6) => totalCartasSalirCarcel (Integer)

; Descripción: Función para el TDA jugador. Crea la estructura de datos del jugador.
; Dominio: id(Integer) X nombre(String) X dinero(Integer) X propiedades(List) X posicion(Integer) X estaEnCarcel(Boolean) X totalCartasSalirCarcel(Integer)
; Recorrido: jugador (Estructura de lista representando al jugador según la definición de representación)
; Tipo recursión: No aplica

(define (jugador id nombre dinero propiedades posicion estaEnCarcel totalCartasSalirCarcel)
  (list id nombre dinero propiedades posicion estaEnCarcel totalCartasSalirCarcel))