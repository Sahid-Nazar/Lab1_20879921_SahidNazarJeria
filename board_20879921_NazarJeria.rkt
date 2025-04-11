#lang racket
(provide tablero)
; Representación TDA Tablero:
; Se utiliza una lista principal que contiene cuatro sub-listas:
; 1. (car lista)      => Lista de propiedades (List of property). La posición se manejará después.
; 2. (cadr lista)     => Mazo de cartas de Suerte (List of carta)
; 3. (caddr lista)    => Mazo de cartas de Comunidad (List of carta)
; 4. (cadddr lista)   => Lista de casillas especiales (List). Puede ser '() inicialmente.

; Descripción: Función constructora para el TDA tablero. Crea la estructura de datos inicial del tablero del juego, conteniendo las listas de propiedades y cartas.
; Dominio: propiedades(List) X cartas-suerte(List) X cartas-comunidad(List) X casillas-especiales(List)
; Recorrido: tablero (Estructura de lista representando el tablero según la definición de representación)
; Tipo recursión: No aplica

(define (tablero propiedades cartas-suerte cartas-comunidad casillas-especiales)
  (list propiedades cartas-suerte cartas-comunidad casillas-especiales))