#lang racket
(require "player_20879921_NazarJeria.rkt")
(require "property_20879921_NazarJeria.rkt")
(require "board_20879921_NazarJeria.rkt")
(require "card_20879921_NazarJeria.rkt")

(provide juego lanzar-dados juego-agregar-jugador
         juego-obtener-jugador-actual juego-lista-jugadores
         juego-tablero juego-dineroBanco juego-numeroDados
         juego-indiceTurnoActual juego-tasaImpuesto juego-maximoCasas
         juego-maximoHoteles juego-actualizar-jugador juego-actualizar-propiedad
         juego-jugar-turno jugador-construir-casa jugador-construir-hotel
         juego-manejar-carcel get-jugador)

; Representación TDA Juego:
; Se utiliza una lista donde cada posición representa:
; 1. Lista de jugadores (List of jugador)
; 2. El tablero (tablero)
; 3. Dinero del Banco (Integer)
; 4. Número de dados por turno (Integer)
; 5. Turno Actual (Integer) ; Índice del jugador actual en la lista de jugadores (0-based)
; 6. Tasa de Impuesto (Integer) ; Podría representar un porcentaje o un valor base
; 7. Máximo de Casas por propiedad (Integer)
; 8. Máximo de Hoteles por propiedad (Integer)
; 9. Estado del Juego (String) ; Ej: "preparacion", "en curso", "terminado"

; Descripción: Función constructora para el TDA juego. Crea la estructura de datos que representa el estado completo de una partida de CAPITALIA.
; Dominio: jugadores(List) X tablero(tablero) X dineroBanco(Integer) X numeroDados(Integer) X turnoActual(Integer) X tasaImpuesto(Integer) X maximoCasas(Integer) X maximoHoteles(Integer) )
; Recorrido: juego (Estructura de lista representando el estado del juego según la definición de representación)
; Tipo recursión: No aplica
(define (juego jugadores tablero dineroBanco numeroDados turnoActual tasaImpuesto maximoCasas maximoHoteles)
  (list jugadores tablero dineroBanco numeroDados turnoActual tasaImpuesto maximoCasas maximoHoteles))



; Descripción: Simula el lanzamiento de un dado utilizando una semilla fija.
; Dominio: semilla (Integer)
; Recorrido: resultado dado (Integer entre 1 y 6)
; Tipo recursión: No aplica
(define (getDadoRandom semilla)
  (cond
    [(= semilla 1) 1]
    [(= semilla 2) 2]
    [(= semilla 5) 3]
    [(= semilla 0) 4]
    [(= semilla 3) 5]
    [(= semilla 4) 6]
    [else (+ 1 (random 6))])) ; Si no es semilla conocida, usamos random



; Descripción: Lanza dos dados usando dos semillas específicas.
; Dominio: semilla1(Integer) x semilla2(Integer)
; Recorrido: Par (Integer . Integer)
; Tipo recursión: No aplica
(define (lanzar-dados semilla1 semilla2)
  (let ((dado1 (getDadoRandom semilla1))
        (dado2 (getDadoRandom semilla2)))
    (display "Valor Dado 1: ") (display dado1) (newline)
    (display "Valor Dado 2: ") (display dado2) (newline)
    (cons dado1 dado2)))

; Selectores TDA Juego

; Descripción: Obtiene la lista de jugadores de la partida.
; Dominio: juego(juego)
; Recorrido: List ; Lista de TDA jugador
; Tipo recursión: No aplica
(define (juego-lista-jugadores un-juego)
  (car un-juego)) ; 1er elemento

; Descripción: Obtiene el TDA tablero de la partida.
; Dominio: juego(juego)
; Recorrido: tablero
; Tipo recursión: No aplica
(define (juego-tablero un-juego)
  (cadr un-juego)) ; 2do elemento

; Descripción: Obtiene la cantidad de dinero actual en el banco.
; Dominio: juego(juego)
; Recorrido: Integer
; Tipo recursión: No aplica
(define (juego-dineroBanco un-juego)
  (caddr un-juego)) ; 3er elemento

; Descripción: Obtiene el número de dados que se usan en la partida.
; Dominio: juego(juego)
; Recorrido: Integer
; Tipo recursión: No aplica
(define (juego-numeroDados un-juego)
  (cadddr un-juego)) ; 4to elemento

; Descripción: Obtiene el índice (0-based) del jugador del turno actual.
; Dominio: juego(juego)
; Recorrido: Integer
; Tipo recursión: No aplica
(define (juego-indiceTurnoActual un-juego) ; Cambié el nombre para claridad
  (list-ref un-juego 4)) ; 5to elemento (índice 4)

; Descripción: Obtiene la tasa de impuesto (probablemente porcentaje).
; Dominio: juego(juego)
; Recorrido: Integer
; Tipo recursión: No aplica
(define (juego-tasaImpuesto un-juego)
  (list-ref un-juego 5)) ; 6to elemento (índice 5)

; Descripción: Obtiene el número máximo de casas construibles por propiedad.
; Dominio: juego(juego)
; Recorrido: Integer
; Tipo recursión: No aplica
(define (juego-maximoCasas un-juego)
  (list-ref un-juego 6)) ; 7mo elemento (índice 6)

; Descripción: Obtiene el número máximo de hoteles construibles por propiedad.
; Dominio: juego(juego)
; Recorrido: Integer
; Tipo recursión: No aplica
(define (juego-maximoHoteles un-juego)
  (list-ref un-juego 7)) ; 8vo elemento (índice 7)



; Descripción: Ejecuta el turno del jugador actual aplicando movimiento, manejo de cárcel, compra de propiedades,
;              pago de renta a otros jugadores, construcción de casas o hoteles, y procesamiento de casillas especiales.
; Dominio: juego (juego) × semillas ((Integer . Integer)) × comprarPropiedad_or_construirCasa (Boolean) ×
;          construirHotel (Boolean) × pagarMultaSalirCarcel (Boolean) × usarTarjetaSalirCarcel (Boolean)
; Recorrido: juego (estado actualizado del juego después del turno)
; Tipo de recursión: No aplica (función secuencial sin recursividad)
(define (juego-jugar-turno juego semillas comprarPropiedad_or_construirCasa construirHotel pagarMultaSalirCarcel usarTarjetaSalirCarcel)
  (let* ((dado1 (getDadoRandom (car semillas)))
         (dado2 (getDadoRandom (cdr semillas)))
         (movimiento (+ dado1 dado2))
         (jugador-actual (juego-obtener-jugador-actual juego)))

    ; Manejo inicial de cárcel
    (define juego-despues-carcel 
      (juego-manejar-carcel juego jugador-actual pagarMultaSalirCarcel usarTarjetaSalirCarcel))

    ; Movimiento del jugador
    (define jugador-actualizado (juego-obtener-jugador-actual juego-despues-carcel))
    (define jugador-movido (jugador-mover jugador-actualizado movimiento))
    (define juego-movido (juego-actualizar-jugador juego-despues-carcel jugador-movido))

    ; Posición y propiedad en la nueva casilla
    (define posicion (jugador-posicion jugador-movido))
    (define propiedad-en-casilla (tablero-obtener-propiedad (juego-tablero juego-movido) posicion))
    (define es-propiedad (pair? propiedad-en-casilla))
    (define propiedad-actual (if es-propiedad (car propiedad-en-casilla) #f))

    (cond
      ; Comprar propiedad sin dueño
      ((and es-propiedad (false? (propiedad-dueño propiedad-actual)) comprarPropiedad_or_construirCasa)
       (if (>= (jugador-dinero jugador-movido) (propiedad-precio propiedad-actual))
           (let ((jugador-con-prop (jugador-comprar-propiedad jugador-movido propiedad-actual))
                 (propiedad-con-dueno (propiedad-actualizar-dueno propiedad-actual (jugador-id jugador-movido))))
             (juego-actualizar-propiedad (juego-actualizar-jugador juego-movido jugador-con-prop) propiedad-con-dueno))
           juego-movido))

      ; Pagar renta a otro jugador
      ((and es-propiedad (propiedad-dueño propiedad-actual)
            (not (= (propiedad-dueño propiedad-actual) (jugador-id jugador-movido))))
       (let* ((jugador-dueno (juego-buscar-jugador-por-id juego-movido (propiedad-dueño propiedad-actual)))
              (renta (propiedad-calcular-renta jugador-dueno propiedad-actual))
              (jugadores-actualizados (jugador-pagar-renta jugador-movido jugador-dueno renta)))
         (juego-actualizar-jugadores-multiples juego-movido jugadores-actualizados)))

      ; Construir casa o hotel en propiedad propia
      ((and es-propiedad (= (propiedad-dueño propiedad-actual) (jugador-id jugador-movido)))
       (let* ((juego-casa (if comprarPropiedad_or_construirCasa
                              (juego-actualizar-propiedad juego-movido (propiedad-construir-casa propiedad-actual (juego-maximoCasas juego-movido)))
                              juego-movido))
              (juego-hotel (if construirHotel
                               (juego-actualizar-propiedad juego-casa (propiedad-construir-hotel propiedad-actual (juego-maximoCasas juego-movido)))
                               juego-casa)))
         juego-hotel))

      ; Casilla especial 
      ((tablero-es-casilla-especial? (juego-tablero juego-movido) posicion)
       juego-movido)

      ; Otra situación 
      (else juego-movido))))




; Descripción: Maneja la situación de un jugador en la cárcel. Permite salir pagando multa o usando una carta "Salir de la cárcel".
; Dominio: juego(juego) X jugador(jugador) X pagarMultaSalirCarcel(Boolean) X usarTarjetaSalirCarcel(Boolean)
; Recorrido: juego (estado actualizado del juego)
; Tipo recursión: No aplica
(define (juego-manejar-carcel juego jugador-actual pagar? usar-tarjeta?)
  (cond
    [usar-tarjeta? 
     (if (> (jugador-totalCartasSalirCarcel jugador-actual) 0)
         (juego-actualizar-jugador 
          juego 
          (jugador (jugador-id jugador-actual)
                   (jugador-nombre jugador-actual)
                   (jugador-dinero jugador-actual)
                   (jugador-propiedades jugador-actual)
                   (jugador-posicion jugador-actual)
                   #f
                   (- (jugador-totalCartasSalirCarcel jugador-actual) 1)))
         juego)]
    [pagar? 
     (let ((jugador-actualizado (jugador (jugador-id jugador-actual)
                                        (jugador-nombre jugador-actual)
                                        (- (jugador-dinero jugador-actual) 50) ; Multa por salir
                                        (jugador-propiedades jugador-actual)
                                        (jugador-posicion jugador-actual)
                                        #f
                                        (jugador-totalCartasSalirCarcel jugador-actual))))
       (juego-actualizar-jugador juego jugador-actualizado))]

    [else juego]))




; Descripción: Agrega un nuevo jugador a la lista.
; Dominio: juego X jugador
; Recorrido: juego actualizado
; Tipo recursión: No aplica
(define (juego-agregar-jugador juego-actual jugador-nuevo)
  (juego (append (juego-lista-jugadores juego-actual) (list jugador-nuevo))
         (juego-tablero juego-actual)
         (juego-dineroBanco juego-actual)
         (juego-numeroDados juego-actual)
         (juego-indiceTurnoActual juego-actual)
         (juego-tasaImpuesto juego-actual)
         (juego-maximoCasas juego-actual)
         (juego-maximoHoteles juego-actual)))



; Descripción: Obtiene el jugador del turno actual.
; Dominio: juego
; Recorrido: jugador
; Tipo recursión: No aplica
(define (juego-obtener-jugador-actual juego-actual)
  (list-ref (juego-lista-jugadores juego-actual) (juego-indiceTurnoActual juego-actual)))



; Descripción: Actualiza un jugador dentro del juego.
; Dominio: juego X jugador
; Recorrido: juego actualizado
; Tipo recursión: No aplica
(define (juego-actualizar-jugador juego-actual jugador-actualizado)
  (let* ((jugadores (juego-lista-jugadores juego-actual))
         (indice (juego-indiceTurnoActual juego-actual))
         (nuevos-jugadores (append (take jugadores indice)
                                   (list jugador-actualizado)
                                   (drop jugadores (+ indice 1)))))
    (juego nuevos-jugadores
           (juego-tablero juego-actual)
           (juego-dineroBanco juego-actual)
           (juego-numeroDados juego-actual)
           (juego-indiceTurnoActual juego-actual)
           (juego-tasaImpuesto juego-actual)
           (juego-maximoCasas juego-actual)
           (juego-maximoHoteles juego-actual))))




; Descripción: Actualiza una propiedad del tablero dentro del juego.
; Dominio: juego X propiedad
; Recorrido: juego actualizado
; Tipo recursión: No aplica
(define (juego-actualizar-propiedad juego-actual propiedad-actualizada)
  (let* ((tablero-actual (juego-tablero juego-actual))
         (propiedades (tablero-propiedades tablero-actual))
         (nuevas-propiedades (map (lambda (par)
                                    (if (= (propiedad-id (car par)) (propiedad-id propiedad-actualizada))
                                        (cons propiedad-actualizada (cdr par))
                                        par))
                                  propiedades))
         (nuevo-tablero (tablero nuevas-propiedades
                                 (tablero-cartas-suerte tablero-actual)
                                 (tablero-cartas-comunidad tablero-actual)
                                 (tablero-casillas-especiales tablero-actual))))
    (juego (juego-lista-jugadores juego-actual)
           nuevo-tablero
           (juego-dineroBanco juego-actual)
           (juego-numeroDados juego-actual)
           (juego-indiceTurnoActual juego-actual)
           (juego-tasaImpuesto juego-actual)
           (juego-maximoCasas juego-actual)
           (juego-maximoHoteles juego-actual))))



; Descripción: Permite construir una casa en la primera propiedad que encuentre del jugador, si no supera el máximo permitido.
; Dominio: jugador
; Recorrido: jugador actualizado
; Tipo recursión: No aplica
(define (jugador-construir-casa jugador)
  (let ((propiedades (jugador-propiedades jugador)))
    (if (null? propiedades)
        jugador ; Si no tiene propiedades, no hace nada
        (let* ((primera-propiedad (car propiedades))
               (nueva-propiedad (propiedad-construir-casa primera-propiedad)))
          (jugador (jugador-id jugador)
                   (jugador-nombre jugador)
                   (jugador-dinero jugador)
                   (cons nueva-propiedad (cdr propiedades))
                   (jugador-posicion jugador)
                   (jugador-estaEnCarcel jugador)
                   (jugador-totalCartasSalirCarcel jugador)))))) 





; Descripción: Permite construir un hotel en la primera propiedad que encuentre del jugador, si cumple las condiciones.
; Dominio: jugador
; Recorrido: jugador actualizado
; Tipo recursión: No aplica
(define (jugador-construir-hotel jugador)
  (let ((propiedades (jugador-propiedades jugador)))
    (if (null? propiedades)
        jugador ; Si no tiene propiedades, no hace nada
        (let* ((primera-propiedad (car propiedades))
               (nueva-propiedad (propiedad-construir-hotel primera-propiedad)))
          (jugador (jugador-id jugador)
                   (jugador-nombre jugador)
                   (jugador-dinero jugador)
                   (cons nueva-propiedad (cdr propiedades))
                   (jugador-posicion jugador)
                   (jugador-estaEnCarcel jugador)
                   (jugador-totalCartasSalirCarcel jugador)))))) 




; Descripción: Busca un jugador por su id dentro de la lista de jugadores del juego.
; Dominio: idJugador(Integer) x juego(juego)
; Recorrido: jugador encontrado
; Tipo recursión: Recursión de cola
(define (juego-buscar-jugador-por-id juego id-buscado)
  (let ((jugadores (juego-lista-jugadores juego)))
    (cond
      [(null? jugadores) #f]
      [(= (jugador-id (car jugadores)) id-buscado) (car jugadores)]
      [else (juego-buscar-jugador-por-id (juego jugadores (juego-tablero juego) (juego-dineroBanco juego) (juego-numeroDados juego) (juego-indiceTurnoActual juego) (juego-tasaImpuesto juego) (juego-maximoCasas juego) (juego-maximoHoteles juego)) id-buscado)])))



; Descripción: Actualiza la lista de jugadores en el juego con una nueva lista de jugadores.
; Dominio: juego(juego) x lista de jugadores actualizada
; Recorrido: juego actualizado
; Tipo recursión: No aplica
(define (juego-actualizar-jugadores-multiples juego-actual jugadores-nuevos)
  (juego jugadores-nuevos
         (juego-tablero juego-actual)
         (juego-dineroBanco juego-actual)
         (juego-numeroDados juego-actual)
         (juego-indiceTurnoActual juego-actual)
         (juego-tasaImpuesto juego-actual)
         (juego-maximoCasas juego-actual)
         (juego-maximoHoteles juego-actual)))



; Descripción: Obtiene un jugador del juego por índice.
; Dominio: juego X índice (Integer)
; Recorrido: jugador
; Tipo recursión: No aplica
(define (get-jugador juego indice)
  (list-ref (juego-lista-jugadores juego) indice))


