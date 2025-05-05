#lang racket
(provide tablero tablero-propiedades tablero-cartas-suerte tablero-cartas-comunidad
         tablero-casillas-especiales tablero-agregar-propiedad tablero-obtener-propiedad
         tablero-es-casilla-especial?)

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

; Selectores TDA Tablero 

; Descripción: Obtiene la lista de propiedades del tablero.
; Dominio: tablero(tablero)
; Recorrido: List ; Lista de TDA propiedad (o pares propiedad.posicion)
; Tipo recursión: No aplica
(define (tablero-propiedades un-tablero)
  (car un-tablero)) ; 1er elemento

; Descripción: Obtiene la lista (mazo) de cartas de Suerte.
; Dominio: tablero(tablero)
; Recorrido: List ; Lista de TDA carta
; Tipo recursión: No aplica
(define (tablero-cartas-suerte un-tablero)
  (cadr un-tablero)) ; 2do elemento

; Descripción: Obtiene la lista (mazo) de cartas de Comunidad.
; Dominio: tablero(tablero)
; Recorrido: List ; Lista de TDA carta
; Tipo recursión: No aplica
(define (tablero-cartas-comunidad un-tablero)
  (caddr un-tablero)) ; 3er elemento

; Descripción: Obtiene la lista de casillas especiales del tablero.
; Dominio: tablero(tablero)
; Recorrido: List ; Lista que representa casillas especiales
; Tipo recursión: No aplica
(define (tablero-casillas-especiales un-tablero)
  (cadddr un-tablero)) ; 4to elemento



; Descripción: Agrega una lista de propiedades al tablero en posiciones específicas.
; Dominio: tablero(tablero) X lista de propiedades-posiciones (List of (cons propiedad . posicion))
; Recorrido: tablero (tablero actualizado)
; Tipo de recursión: No aplica
(define (tablero-agregar-propiedad un-tablero propiedades-nuevas)
  (tablero
   (append (tablero-propiedades un-tablero) propiedades-nuevas)
   (tablero-cartas-suerte un-tablero)
   (tablero-cartas-comunidad un-tablero)
   (tablero-casillas-especiales un-tablero)))



; Descripción: Obtiene la propiedad en una posición específica del tablero (si existe)
; Dominio: tablero(tablero) X posicion(Integer)
; Recorrido: (Pair propiedad Integer) o #f si no hay propiedad en esa posición
; Tipo recursión: Recursión de cola
(define (tablero-obtener-propiedad un-tablero posicion)
  (let buscar ((propiedades (tablero-propiedades un-tablero)))
    (cond
      [(null? propiedades) #f] ; No hay propiedad en esa posición
      [(= (cdr (car propiedades)) posicion) (car propiedades)] ; Devuelve el par (propiedad . posición)
      [else (buscar (cdr propiedades))]))) 


; Descripción: Verifica si en una posición del tablero hay una casilla especial.
; Dominio: tablero(tablero) X posicion(Integer)
; Recorrido: Boolean
; Tipo recursión: No aplica
(define (tablero-es-casilla-especial? un-tablero posicion)
  (let ((casillas (tablero-casillas-especiales un-tablero)))
    (cond
      [(null? casillas) #f]
      [(= (cdr (car casillas)) posicion) #t]
      [else (tablero-es-casilla-especial? (tablero (tablero-propiedades un-tablero)
                                                   (tablero-cartas-suerte un-tablero)
                                                   (tablero-cartas-comunidad un-tablero)
                                                   (cdr casillas))
                                          posicion)])))




