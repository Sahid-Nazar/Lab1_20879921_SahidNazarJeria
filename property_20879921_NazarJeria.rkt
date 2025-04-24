#lang racket
(provide propiedad
         propiedad-id propiedad-nombre propiedad-precio propiedad-renta
         propiedad-dueño propiedad-casas propiedad-esHotel propiedad-estaHipotecada
         propiedad-calcular-renta propiedad-hipotecar propiedad-construir-casa
         propiedad-construir-hotel)
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



; Descripción: Calcula el monto de la renta a pagar por una propiedad, considerando las casas/hotel construidos.
;              renta con hotel = 2 * renta con 4 casas
; Dominio: jugador(jugador) X propiedad(propiedad) 
; Recorrido: Integer (Monto de la renta a pagar)
; Tipo recursión: No aplica
(define (propiedad-calcular-renta un-jugador una-propiedad)
  (let* ((renta-base (propiedad-renta una-propiedad))
         (num-casas (propiedad-casas una-propiedad))
         (hay-hotel (propiedad-esHotel una-propiedad)))
    (cond
      [(propiedad-estaHipotecada una-propiedad) 0]
      [hay-hotel (* renta-base 2 (+ 1 4))] ; 2 veces la renta máxima con 4 casas
      [else (* renta-base (+ 1 num-casas))])))



; Descripción: Cambia el estado de una propiedad a hipotecada. Devuelve un nuevo TDA propiedad con el estado actualizado.
; Dominio: propiedad(propiedad)
; Recorrido: propiedad (Un nuevo TDA propiedad con el campo 'estaHipotecada' puesto a #t)
; Tipo recursión: No aplica
(define (propiedad-hipotecar propiedad-a-hipotecar)
  ; Construimos y devolvemos una nueva propiedad igual a la original,
  ; pero con el último campo (estaHipotecada) puesto a #t.
  (propiedad (propiedad-id propiedad-a-hipotecar)        
             (propiedad-nombre propiedad-a-hipotecar)    
             (propiedad-precio propiedad-a-hipotecar)   
             (propiedad-renta propiedad-a-hipotecar)     
             (propiedad-dueño propiedad-a-hipotecar)     
             (propiedad-casas propiedad-a-hipotecar)     
             (propiedad-esHotel propiedad-a-hipotecar)   
             #t))


; Descripción: Aumenta en 1 el número de casas de una propiedad, si no supera el máximo definido por el juego.
; Dominio: propiedad(propiedad) X juego(juego)
; Recorrido: propiedad
; Tipo recursión: No aplica
(define (propiedad-construir-casa prop juego)
  (let* ((casas (propiedad-casas prop))
         (max-casas (juego-maximoCasas juego)))
    (if (< casas max-casas)
        (propiedad (propiedad-id prop)
                   (propiedad-nombre prop)
                   (propiedad-precio prop)
                   (propiedad-renta prop)
                   (propiedad-dueño prop)
                   (+ casas 1)
                   (propiedad-esHotel prop)
                   (propiedad-estaHipotecada prop))
        prop)))



; Descripción: Convierte las casas de una propiedad en un hotel si tiene el máximo de casas.
; Dominio: propiedad(propiedad) X juego(juego)
; Recorrido: propiedad
; Tipo recursión: No aplica
(define (propiedad-construir-hotel prop juego)
  (if (= (propiedad-casas prop) (juego-maximoCasas juego))
      (propiedad (propiedad-id prop)
                 (propiedad-nombre prop)
                 (propiedad-precio prop)
                 (propiedad-renta prop)
                 (propiedad-dueño prop)
                 0                ; casas = 0
                 #t               ; esHotel = true
                 (propiedad-estaHipotecada prop))
      prop))
