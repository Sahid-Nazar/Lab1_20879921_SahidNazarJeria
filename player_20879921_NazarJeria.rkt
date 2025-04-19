#lang racket
(provide jugador jugador-mover jugador-comprar-propiedad
         jugador-id jugador-nombre jugador-dinero jugador-propiedades
         jugador-posicion jugador-estaEnCarcel jugador-totalCartasSalirCarcel)
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



; Descripción: Calcula la nueva posición de un jugador en el tablero después de lanzar los dados, considerando el movimiento circular (asumiendo 40 casillas). Devuelve un nuevo TDA jugador con la posición actualizada.
; Dominio: jugador(jugador) X valoresDados(Pair) X juego(juego) ; valoresDados es (dado1 . dado2)
; Recorrido: jugador (Un nuevo TDA jugador con el campo 'posicion' actualizado)
; Tipo recursión: No aplica
(define (jugador-mover jugador-actual valoresDados juego-actual)
  ; Asumimos tamaño del tablero = 40 para el movimiento circular
  (let* (
         ; Extracción usando Selectores 
         (id                (jugador-id jugador-actual)) ; <-- Usamos selector
         (nombre            (jugador-nombre jugador-actual)) ; <-- Usamos selector
         (dinero            (jugador-dinero jugador-actual)) ; <-- Usamos selector
         (propiedades       (jugador-propiedades jugador-actual)) ; <-- Usamos selector
         (posicion-vieja    (jugador-posicion jugador-actual)) ; <-- Usamos selector
         (estaEnCarcel      (jugador-estaEnCarcel jugador-actual)) ; <-- Usamos selector
         (totalCartas       (jugador-totalCartasSalirCarcel jugador-actual)) ; <-- Usamos selector

         ; Datos del lanzamiento de dados (estos no usan selectores de jugador)
         (dado1             (car valoresDados))
         (dado2             (cdr valoresDados))
         (suma-dados        (+ dado1 dado2))

         ; Cálculo de la nueva posición
         (nueva-posicion    (modulo (+ posicion-vieja suma-dados) 40))
        ) 

    ; Construimos y devolvemos el nuevo jugador
    ; Llamamos al constructor 'jugador' 
    (jugador id
             nombre
             dinero
             propiedades
             nueva-posicion   ; <-- La posición actualizada
             estaEnCarcel
             totalCartas)
  ))



; Descripción: Permite a un jugador comprar una propiedad. Verifica si tiene dinero suficiente. Si puede comprar, devuelve un nuevo TDA jugador con el dinero descontado y la propiedad añadida a su lista. Si no puede, devuelve el jugador original sin cambios.
; Dominio: jugador(jugador) X propiedad(propiedad)
; Recorrido: jugador (El TDA del jugador actualizado o el original si no hay fondos)
; Tipo recursión: No aplica
(define (jugador-comprar-propiedad jugador-actual propiedad-a-comprar)
  (let* (
         ; Datos del jugador actual usando selectores
         (id                   (jugador-id jugador-actual))
         (nombre               (jugador-nombre jugador-actual))
         (dinero-jugador       (jugador-dinero jugador-actual)) 
         (propiedades-viejas   (jugador-propiedades jugador-actual)) 
         (posicion             (jugador-posicion jugador-actual)) 
         (estaEnCarcel         (jugador-estaEnCarcel jugador-actual)) 
         (totalCartas          (jugador-totalCartasSalirCarcel jugador-actual)) 

         ; Dato de la propiedad usando selector 
         (precio-propiedad     (propiedad-precio propiedad-a-comprar)) ; Usa selector de propiedad
        )

    ; Hacemos la condicion para saber si el jugador tiene suficiente dinero
    (if (>= dinero-jugador precio-propiedad)
        ; Si puede comprar, entonces calcula nuevos valores y construye nuevo jugador
        (let* ((dinero-nuevo       (- dinero-jugador precio-propiedad))
               (propiedades-nuevas (cons propiedad-a-comprar propiedades-viejas))) ; Añade al principio

          ; Construimos y devolvemos el nuevo jugador con datos actualizados
          (jugador id
                   nombre
                   dinero-nuevo         ; <-- Dinero actualizado
                   propiedades-nuevas   ; <-- Lista de propiedades actualizada
                   posicion
                   estaEnCarcel
                   totalCartas))
        ; Sino puede comprar, entonces devolvemos el jugador original sin modificaciones
        jugador-actual
     ) 
   ) 
) 