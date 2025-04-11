#lang racket
(provide propiedad)
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



