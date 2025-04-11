#lang racket
(provide carta)
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

(define(carta id tipo descripcion accion)
  (list id tipo descripcion accion))