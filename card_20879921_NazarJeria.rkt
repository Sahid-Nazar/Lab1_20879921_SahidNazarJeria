#lang racket
(provide carta 
         carta-id carta-tipo carta-descripcion carta-accion)

; Representación TDA Carta:
; Se utiliza una lista donde cada posición representa:
; 1. id (Integer)
; 2. tipo (String) ; "suerte" o "comunidad"
; 3. descripcion (String) ; Texto que se muestra al jugador
; 4. accion (Symbol) ; Símbolo que identifica la acción a realizar (ej: 'pay-tax)

; Descripción: Función constructora para el TDA carta. Crea la estructura de datos de una carta del juego (Suerte o Comunidad).
; Dominio: id(Integer) X tipo(String) X descripcion(String) X accion(Symbol)
; Recorrido: carta (Estructura de lista representando la carta según la definición de representación)
; Tipo recursión: No aplica
(define (carta id tipo descripcion accion)
  (list id tipo descripcion accion))

; Selectores del TDA Carta:

; Descripción: Obtiene el ID único de la carta.
; Dominio: carta(carta)
; Recorrido: Integer
; Tipo recursión: No aplica
(define (carta-id una-carta)
  (car una-carta))

; Descripción: Obtiene el tipo de la carta ("suerte" o "comunidad").
; Dominio: carta(carta)
; Recorrido: String
; Tipo recursión: No aplica
(define (carta-tipo una-carta)
  (cadr una-carta))

; Descripción: Obtiene la descripción de la carta (texto explicativo).
; Dominio: carta(carta)
; Recorrido: String
; Tipo recursión: No aplica
(define (carta-descripcion una-carta)
  (caddr una-carta))

; Descripción: Obtiene la acción asociada a la carta.
; Dominio: carta(carta)
; Recorrido: Symbol
; Tipo recursión: No aplica
(define (carta-accion una-carta)
  (cadddr una-carta))
