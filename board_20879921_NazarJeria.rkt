#lang racket
(provide tablero tablero-agregar-propiedades
         tablero-propiedades tablero-cartas-suerte
         tablero-cartas-comunidad tablero-casillas-especiales)
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



; Descripción: Agrega una lista de propiedades (cada una asociada a una posición) al TDA tablero. Devuelve un nuevo tablero con las propiedades añadidas.
; Dominio: tablero(tablero) X propiedades-con-posicion(List) ; donde cada elemento es un par (propiedad . posicion)
; Recorrido: tablero (Una nueva estructura de lista representando el tablero actualizado)
; Tipo recursión: No aplica
(define (tablero-agregar-propiedades tablero-actual lista-propiedades-con-posicion)
  ; Lo primero que haremos sera desarmar el tablero viejo extrayendo las listas que ya estaban en el tablero
  (let ((propiedades-viejas (car tablero-actual))
        (cartas-suerte      (cadr tablero-actual))
        (cartas-comunidad   (caddr tablero-actual))
        (casillas-especiales (cadddr tablero-actual)))
    ; Ahora creamos una lista que contenga SÓLO las propiedades de la lista de entrada
    ; (ignorando la posición por ahora, usando 'map' y 'car')
    (let* ((propiedades-nuevas-solas   (map car lista-propiedades-con-posicion))
           ; Unimos propiedades viejas con propiedades nuevas 
           (propiedades-actualizadas (append propiedades-viejas propiedades-nuevas-solas)))
      ; LLamamos a la funcion constructora para armar el tablero con las entradas actualizadas
      (tablero propiedades-actualizadas cartas-suerte cartas-comunidad casillas-especiales))))
