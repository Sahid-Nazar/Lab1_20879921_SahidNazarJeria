#lang racket
(provide jugador jugador-mover jugador-comprar-propiedad jugador-pagar-renta
         jugador-id jugador-nombre jugador-dinero jugador-propiedades
         jugador-posicion jugador-estaEnCarcel jugador-totalCartasSalirCarcel jugador-esta-en-bancarrota)
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

; Selectores TDA Jugador (Modificacion por la actualizacion de las instrucciones)

; Descripción: Obtiene el ID único del jugador.
; Dominio: jugador(jugador)
; Recorrido: Integer
; Tipo recursión: No aplica
(define (jugador-id un-jugador)
  (car un-jugador))

; Descripción: Obtiene el nombre del jugador.
; Dominio: jugador(jugador)
; Recorrido: String
; Tipo recursión: No aplica
(define (jugador-nombre un-jugador)
  (cadr un-jugador))

; Descripción: Obtiene la cantidad de dinero actual del jugador.
; Dominio: jugador(jugador)
; Recorrido: Integer
; Tipo recursión: No aplica
(define (jugador-dinero un-jugador)
  (caddr un-jugador))

; Descripción: Obtiene la lista de propiedades (TDAs propiedad) que posee el jugador.
; Dominio: jugador(jugador)
; Recorrido: List
; Tipo recursión: No aplica
(define (jugador-propiedades un-jugador)
  (cadddr un-jugador))

; Descripción: Obtiene la posición actual del jugador en el tablero (índice numérico).
; Dominio: jugador(jugador)
; Recorrido: Integer
; Tipo recursión: No aplica
(define (jugador-posicion un-jugador)
  (list-ref un-jugador 4))

; Descripción: Verifica si el jugador se encuentra actualmente en la cárcel.
; Dominio: jugador(jugador)
; Recorrido: Boolean
; Tipo recursión: No aplica
(define (jugador-estaEnCarcel un-jugador)
  (list-ref un-jugador 5))

; Descripción: Obtiene la cantidad de cartas "Salir de la Cárcel Gratis" que posee el jugador.
; Dominio: jugador(jugador)
; Recorrido: Integer
; Tipo recursión: No aplica
(define (jugador-totalCartasSalirCarcel un-jugador)
  (list-ref un-jugador 6))




; Descripción: Calcula la nueva posición de un jugador en el tablero después de lanzar los dados.
; Dominio: jugador(jugador) X movimiento(Integer)
; Recorrido: jugador
; Tipo recursión: No aplica
(define (jugador-mover jugador-actual movimiento)
  (let* ((id (jugador-id jugador-actual))
         (nombre (jugador-nombre jugador-actual))
         (dinero (jugador-dinero jugador-actual))
         (propiedades (jugador-propiedades jugador-actual))
         (posicion-actual (jugador-posicion jugador-actual))
         (estaEnCarcel (jugador-estaEnCarcel jugador-actual))
         (totalCartas (jugador-totalCartasSalirCarcel jugador-actual))
         (nueva-posicion (modulo (+ posicion-actual movimiento) 40))) ; 40 casillas en el tablero
    (jugador id nombre dinero propiedades nueva-posicion estaEnCarcel totalCartas)))





; Descripción: Permite comprar una propiedad si hay dinero suficiente.
; Dominio: jugador X propiedad
; Recorrido: jugador actualizado
; Tipo recursión: No aplica
(define (jugador-comprar-propiedad jugador-actual propiedad)
  (let ((precio (list-ref propiedad 2))) ; precio de la propiedad
    (if (>= (jugador-dinero jugador-actual) precio)
        (jugador (jugador-id jugador-actual)
                 (jugador-nombre jugador-actual)
                 (- (jugador-dinero jugador-actual) precio)
                 (cons propiedad (jugador-propiedades jugador-actual))
                 (jugador-posicion jugador-actual)
                 (jugador-estaEnCarcel jugador-actual)
                 (jugador-totalCartasSalirCarcel jugador-actual))
        jugador-actual)))


; Descripción: Transfiere un monto entre jugadores si el pagador tiene suficiente dinero.
; Dominio: jugador X jugador X monto
; Recorrido: lista de dos jugadores actualizados
; Tipo recursión: No aplica
(define (jugador-pagar-renta pagador receptor monto)
  (if (>= (jugador-dinero pagador) monto)
      (list (jugador (jugador-id pagador)
                     (jugador-nombre pagador)
                     (- (jugador-dinero pagador) monto)
                     (jugador-propiedades pagador)
                     (jugador-posicion pagador)
                     (jugador-estaEnCarcel pagador)
                     (jugador-totalCartasSalirCarcel pagador))
            (jugador (jugador-id receptor)
                     (jugador-nombre receptor)
                     (+ (jugador-dinero receptor) monto)
                     (jugador-propiedades receptor)
                     (jugador-posicion receptor)
                     (jugador-estaEnCarcel receptor)
                     (jugador-totalCartasSalirCarcel receptor)))
      (list pagador receptor)))


; Descripción: Verifica si un jugador está en bancarrota (sin dinero).
; Dominio: jugador
; Recorrido: boolean
; Tipo recursion: No aplica
(define (jugador-esta-en-bancarrota jugador)
  (<= (jugador-dinero jugador) 0))


; Descripción: Obtiene la cantidad de cartas 'Salir de la Cárcel' que tiene un jugador.
; Dominio: jugador(jugador)
; Recorrido: Integer
; Tipo recursión: No aplica
(define (jugador-totalCartasSalirEnCarcel un-jugador)
  (list-ref un-jugador 6))
