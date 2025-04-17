#lang racket
(provide propiedad
         propiedad-id propiedad-nombre propiedad-precio propiedad-renta
         propiedad-dueño propiedad-casas propiedad-esHotel propiedad-estaHipotecada)
; Representación TDA Propiedad:
; Se utiliza una lista donde cada posición representa:
; 1. id (Integer)
; 2. nombre (String)
; 3. precio (Integer)
; 4. renta (Integer)
; 5. dueño (jugador O #f) ;  #f Lo usaremos para indicar que no tiene dueño (propiedad del Banco)
; 6. casas (Integer)
; 7. esHotel (Boolean)
; 8. estaHipotecada (Boolean)

; Descripción: Función para el TDA propiedad. Crea la estructura de datos de la propiedad.
; Dominio: id(Integer) X nombre(String) X precio(Integer) X renta(Integer) X dueño(jugador/#f) X casas(Integer) X esHotel(Boolean) X estaHipotecada(Boolean)
; Recorrido: propiedad (Estructura para propiedad representando al jugador según la definición de representación)
; Tipo recursión: No aplica

(define(propiedad id nombre precio renta dueño casas esHotel estaHipotecada)
  (list id nombre precio renta dueño casas esHotel estaHipotecada))



; Selectores TDA Propiedad

; Descripción: Obtiene el ID único de la propiedad.
; Dominio: propiedad(propiedad)
; Recorrido: Integer
; Tipo recursión: No aplica
(define (propiedad-id una-propiedad)
  (car una-propiedad))

; Descripción: Obtiene el nombre de la propiedad.
; Dominio: propiedad(propiedad)
; Recorrido: String
; Tipo recursión: No aplica
(define (propiedad-nombre una-propiedad)
  (cadr una-propiedad)) 

; Descripción: Obtiene el precio de compra de la propiedad.
; Dominio: propiedad(propiedad)
; Recorrido: Integer
; Tipo recursión: No aplica
(define (propiedad-precio una-propiedad)
  (caddr una-propiedad)) 

; Descripción: Obtiene el monto de la renta base de la propiedad.
; Dominio: propiedad(propiedad)
; Recorrido: Integer
; Tipo recursión: No aplica
(define (propiedad-renta una-propiedad)
  (cadddr una-propiedad)) 

; Descripción: Obtiene el dueño actual de la propiedad (TDA jugador o #f si no tiene).
; Dominio: propiedad(propiedad)
; Recorrido: jugador O #f
; Tipo recursión: No aplica
(define (propiedad-dueño una-propiedad)
  (list-ref una-propiedad 4)) ; 5to elemento (índice 4)

; Descripción: Obtiene el número de casas construidas en la propiedad.
; Dominio: propiedad(propiedad)
; Recorrido: Integer
; Tipo recursión: No aplica
(define (propiedad-casas una-propiedad)
  (list-ref una-propiedad 5)) ; 6to elemento (índice 5)

; Descripción: Verifica si hay un hotel construido en la propiedad.
; Dominio: propiedad(propiedad)
; Recorrido: Boolean
; Tipo recursión: No aplica
(define (propiedad-esHotel una-propiedad)
  (list-ref una-propiedad 6)) ; 7mo elemento (índice 6)

; Descripción: Verifica si la propiedad está actualmente hipotecada.
; Dominio: propiedad(propiedad)
; Recorrido: Boolean
; Tipo recursión: No aplica
(define (propiedad-estaHipotecada una-propiedad)
  (list-ref una-propiedad 7)) ; 8vo elemento (índice 7)