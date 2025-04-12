#lang racket
(provide juego juego-agregar-jugador)
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
    