#lang racket
(provide juego juego-agregar-jugador juego-obtener-jugador-actual
         juego-lista-jugadores juego-tablero juego-dineroBanco juego-numeroDados
         juego-indiceTurnoActual juego-tasaImpuesto juego-maximoCasas
         juego-maximoHoteles juego-estadoJuego)
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
; Dominio: jugadores(List) X tablero(tablero) X dineroBanco(Integer) X numeroDados(Integer) X turnoActual(Integer) X tasaImpuesto(Integer) X maximoCasas(Integer) X maximoHoteles(Integer) X estadoJuego(String)
; Recorrido: juego (Estructura de lista representando el estado del juego según la definición de representación)
; Tipo recursión: No aplica
(define (juego jugadores tablero dineroBanco numeroDados turnoActual tasaImpuesto maximoCasas maximoHoteles estadoJuego)
  (list jugadores tablero dineroBanco numeroDados turnoActual tasaImpuesto maximoCasas maximoHoteles estadoJuego))


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

; Descripción: Obtiene el estado actual de la partida (ej: "preparacion", "en curso").
; Dominio: juego(juego)
; Recorrido: String
; Tipo recursión: No aplica
(define (juego-estadoJuego un-juego)
  (list-ref un-juego 8)) ; 9no elemento (índice 8)



; Descripción: Agrega un jugador a la lista de jugadores de la partida. Devuelve un nuevo estado del juego.
; Dominio: juego(juego) X jugador(jugador) ; Se asume que el jugador ya tiene el capital inicial.
; Recorrido: juego (Una nueva estructura de lista representando el estado del juego actualizado)
; Tipo recursión: No aplica
(define (juego-agregar-jugador juego-actual jugador-nuevo)
  (let* (
         ; Extraemos todas las partes necesarias
         ; La lista de jugadores vieja
         (jugadores-viejos      (car juego-actual)) ; 1er elemento
         ; Las partes que no cambian
         (tablero               (cadr juego-actual))  ; 2do elemento
         (dineroBanco           (caddr juego-actual)) ; 3er elemento
         (numeroDados           (cadddr juego-actual)); 4to elemento
         ; Para el resto, usamos list-ref considerando que el 1er elemento es indice 0
         (turnoActual           (list-ref juego-actual 4)) ; 5to elemento (índice 4)
         (tasaImpuesto          (list-ref juego-actual 5)) ; 6to elemento (índice 5)
         (maximoCasas           (list-ref juego-actual 6)) ; 7mo elemento (índice 6)
         (maximoHoteles         (list-ref juego-actual 7)) ; 8vo elemento (índice 7)
         (estadoJuego           (list-ref juego-actual 8)) ; 9no elemento (índice 8)

         ; Añadimos el jugador-nuevo (dentro de una lista) al final de la lista vieja
         (jugadores-actualizados (append jugadores-viejos (list jugador-nuevo)))
         )

    ; Construimos y devolvemos el nuevo estado del juego 
    (juego jugadores-actualizados
           tablero          ; Los pusimos hacia abajo para que no quedara una linea tan larga
           dineroBanco
           numeroDados
           turnoActual
           tasaImpuesto
           maximoCasas
           maximoHoteles
           estadoJuego)
    ))



; Descripción: Obtiene el TDA del jugador cuyo turno está actualmente en curso.
; Dominio: juego(juego)
; Recorrido: jugador (El TDA del jugador actual)
; Tipo recursión: No aplica
(define (juego-obtener-jugador-actual juego-actual)
  ; Accedemos a la lista de jugadores 
  ; y luego obtenemos el elemento en el índice del turno actual que corresponde al indice 4 (list-ref juego-actual 4)
  (list-ref (car juego-actual) (list-ref juego-actual 4)))

















