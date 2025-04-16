#lang racket
(provide jugador jugador-mover jugador-comprar-propiedad)
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



; Descripción: Calcula la nueva posición de un jugador en el tablero después de lanzar los dados, considerando el movimiento circular (asumiendo 40 casillas). Devuelve un nuevo TDA jugador con la posición actualizada.
; Dominio: jugador(jugador) X valoresDados(Pair) X juego(juego) ; valoresDados es (dado1 . dado2)
; Recorrido: jugador (Un nuevo TDA jugador con el campo 'posicion' actualizado)
; Tipo recursión: No aplica
(define (jugador-mover jugador-actual valoresDados juego-actual)
  ; Asumimos tamaño del tablero = 40 para el movimiento circular
  (let* ( 
         ; Datos del jugador que no cambian 
         (id                (car jugador-actual))        ; 1er elemento
         (nombre            (cadr jugador-actual))       ; 2do elemento
         (dinero            (caddr jugador-actual))      ; 3er elemento
         (propiedades       (cadddr jugador-actual))     ; 4to elemento
         (estaEnCarcel      (list-ref jugador-actual 5)) ; 6to elemento (índice 5)
         (totalCartas       (list-ref jugador-actual 6)) ; 7mo elemento (índice 6)

         ; Datos que si usamos para calcular 
         (posicion-vieja    (list-ref jugador-actual 4)) ; 5to elemento (índice 4)
         (dado1             (car valoresDados))          ; Sacamos el valor del primer dado del par
         (dado2             (cdr valoresDados))          ; Sacamos el valor del segundo dado del par
         (suma-dados        (+ dado1 dado2))             ; Sumamos los dados

         ; Cálculamos la nueva posición
         ; Sumamos la posición vieja y los dados, y usamos módulo 40 para la vuelta
         (nueva-posicion    (modulo (+ posicion-vieja suma-dados) 40))
        ) ; Fin let*

    ; Construimos y devolvemos el nuevo jugador 
    ; Llamamos al constructor 'jugador' con los datos originales,
    ; excepto por la posición, donde usamos la nueva para que este actualizada.
    (jugador id                 
             nombre           
             dinero           
             propiedades      
             nueva-posicion   
             estaEnCarcel     
             totalCartas)     
  )) ; Se mantienen todos los parametros iguales, menos la nueva posicion que estara actualizada



; Descripción: Permite a un jugador comprar una propiedad. Verifica si tiene dinero suficiente. Si puede comprar, devuelve un nuevo TDA jugador con el dinero descontado y la propiedad añadida a su lista. Si no puede, devuelve el jugador original sin cambios.
; Dominio: jugador(jugador) X propiedad(propiedad)
; Recorrido: jugador (El TDA del jugador actualizado o el original si no hay fondos)
; Tipo recursión: No aplica
(define (jugador-comprar-propiedad jugador-actual propiedad-a-comprar)
  (let* (
         ; Datos del jugador actual
         (id                   (car jugador-actual))        ; No cambia
         (nombre               (cadr jugador-actual))       ; No cambia
         (dinero-jugador       (caddr jugador-actual))      ; Necesario para comparar y calcular
         (propiedades-viejas   (cadddr jugador-actual))     ; Necesario para añadir la nueva
         (posicion             (list-ref jugador-actual 4)) ; No cambia
         (estaEnCarcel         (list-ref jugador-actual 5)) ; No cambia
         (totalCartas          (list-ref jugador-actual 6)) ; No cambia

         ; Datos de la propiedad que se quiere comprar
         ; Asumimos que el precio es el 3er elemento (caddr) del TDA propiedad
         (precio-propiedad     (caddr propiedad-a-comprar))
        ) 

    ; Hacemos la condicion para saber si el jugador tiene suficiente dinero
    (if (>= dinero-jugador precio-propiedad)
        ; Calcula el nuevo dinero y la nueva lista de propiedades
        (let* ((dinero-nuevo       (- dinero-jugador precio-propiedad))
               ; Añadimos la propiedad nueva al principio de la lista vieja 
               (propiedades-nuevas (cons propiedad-a-comprar propiedades-viejas)))

          ; Construimos y devolvemos el nuevo jugador con datos actualizados
          (jugador id
                   nombre
                   dinero-nuevo         ; Dinero actualizado
                   propiedades-nuevas   ; Lista de propiedades actualizada
                   posicion
                   estaEnCarcel
                   totalCartas))
        ; Parte del else
        ; Devolvemos el jugador original sin  modificaciones
        jugador-actual
     ) 
   ) 
) 