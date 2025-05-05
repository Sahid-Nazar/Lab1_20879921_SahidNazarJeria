#lang racket
(provide propiedad
         propiedad-id propiedad-nombre propiedad-precio propiedad-renta
         propiedad-dueño propiedad-casas propiedad-esHotel propiedad-estaHipotecada
         propiedad-calcular-renta propiedad-hipotecar propiedad-construir-casa
         propiedad-construir-hotel propiedad-actualizar-dueno)
; Representación TDA Propiedad:
; Se utiliza una lista donde cada posición representa:
; 1. id (Integer)
; 2. nombre (String)
; 3. precio (Integer)
; 4. renta (Integer)
; 5. dueño (jugador O #f) 
; 6. casas (Integer)
; 7. esHotel (Boolean)
; 8. estaHipotecada (Boolean)



; Descripción: Constructor del TDA propiedad, crea una propiedad en el juego con sus respectivos atributos.
; Dominio: id(int) X nombre(string) X precio(int) X renta(int) X dueño(int or #f) X casas(int) X esHotel(bool) X estaHipotecada(bool)
; Recorrido: propiedad (lista con los datos especificados)
; Tipo de recursión: No aplica
(define (propiedad id nombre precio renta dueño casas esHotel estaHipotecada)
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

; Descripción: Obtiene el precio de compra de la propiedad si es lista válida.
; Dominio: propiedad(propiedad)
; Recorrido: Integer
; Tipo recursión: No aplica
(define (propiedad-precio una-propiedad)
  (if (list? una-propiedad)
      (caddr una-propiedad)
      (error 'propiedad-precio "Error, Se esperaba una propiedad válida, no un número")))
 

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
  (if (list? una-propiedad)
      (list-ref una-propiedad 4)
      (error 'propiedad-dueño "Error, Se esperaba una propiedad válida, no un número")))
 

; Descripción: Obtiene el número de casas construidas en la propiedad.
; Dominio: propiedad(propiedad)
; Recorrido: Integer
; Tipo recursión: No aplica
(define (propiedad-casas una-propiedad)
  (list-ref una-propiedad 5)) 

; Descripción: Verifica si hay un hotel construido en la propiedad.
; Dominio: propiedad(propiedad)
; Recorrido: Boolean
; Tipo recursión: No aplica
(define (propiedad-esHotel una-propiedad)
  (list-ref una-propiedad 6)) 

; Descripción: Verifica si la propiedad está actualmente hipotecada.
; Dominio: propiedad(propiedad)
; Recorrido: Boolean
; Tipo recursión: No aplica
(define (propiedad-estaHipotecada una-propiedad)
  (list-ref una-propiedad 7)) 



; Descripción: Calcula la renta de una propiedad dependiendo de casas/hotel.
; Dominio: jugador X propiedad
; Recorrido: Integer
; Tipo recursión: No aplica
(define (propiedad-calcular-renta un-jugador una-propiedad)
  (let ((renta-base (propiedad-renta una-propiedad))
        (casas (propiedad-casas una-propiedad))
        (hotel (propiedad-esHotel una-propiedad)))
    (cond  
      [(propiedad-estaHipotecada una-propiedad) 0]
      [hotel (* renta-base 10)]
      [else (* renta-base (+ 1 casas))])))



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


; Descripción: Aumenta en 1 el número de casas de una propiedad si no supera el máximo permitido.
; Dominio: propiedad X max-casas(Integer)
; Recorrido: propiedad
; Tipo recursión: No aplica
(define (propiedad-construir-casa propiedad max-casas)
  (if (not (propiedad-estaHipotecada propiedad))
      (if (< (propiedad-casas propiedad) max-casas)
          (propiedad (propiedad-id propiedad)
                     (propiedad-nombre propiedad)
                     (propiedad-precio propiedad)
                     (propiedad-renta propiedad)
                     (propiedad-dueño propiedad)
                     (+ (propiedad-casas propiedad) 1)
                     (propiedad-esHotel propiedad)
                     (propiedad-estaHipotecada propiedad))
          propiedad)
      propiedad))



; Descripción: Convierte una propiedad en un hotel si tiene el máximo de casas permitido.
; Dominio: propiedad X max-casas(Integer)
; Recorrido: propiedad
; Tipo recursión: No aplica
(define (propiedad-construir-hotel propiedad max-casas)
  (if (not (propiedad-estaHipotecada propiedad))
      (if (= (propiedad-casas propiedad) max-casas)
          (propiedad (propiedad-id propiedad)
                     (propiedad-nombre propiedad)
                     (propiedad-precio propiedad)
                     (propiedad-renta propiedad)
                     (propiedad-dueño propiedad)
                     0
                     #t
                     (propiedad-estaHipotecada propiedad))
          propiedad)
      propiedad))




; Descripción: Modifica el dueño de la propiedad
; Dominio: propiedad(propiedad) X nuevo-dueño(int)
; Recorrido: propiedad actualizada
; Tipo recursión: No aplica
(define (propiedad-actualizar-dueno prop nuevo-dueno)
  (propiedad (propiedad-id prop) (propiedad-nombre prop) (propiedad-precio prop)
             (propiedad-renta prop) nuevo-dueno (propiedad-casas prop)
             (propiedad-esHotel prop) (propiedad-estaHipotecada prop)))



