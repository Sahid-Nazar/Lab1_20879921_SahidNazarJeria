#lang racket
(provide juego)
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