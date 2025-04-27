#lang racket
(provide juego-lanzar-dados)
; Descripción: Simula el lanzamiento de 2 dados (valores aleatorios entre 1 y 6). Imprime por pantalla el valor de cada dado y devuelve un par con ambos valores.
; Dominio:  (No recibe argumentos)
; Recorrido: Par (Integer . Integer) ; Par con los valores de los dos dados (dado1 . dado2)
; Tipo recursión: No aplica
(define (juego-lanzar-dados)
  ; Con let guardamos los resultados de cada dado
  (let ((dado1 (+ 1 (random 6))) ; Generamos un número entre 1 y 6 para el dado 1
        (dado2 (+ 1 (random 6)))) ; Lo mismo, pero para el dado 2

    ; Mostramos los resultados por pantalla 
    (display "Valor Dado 1: ")
    (display dado1)
    (newline) ; Salto de línea para que se vea mejor
    (display "Valor Dado 2: ")
    (display dado2)
    (newline)

    ; Creamos y devolvemos el par con los dos valores de los dados
    (cons dado1 dado2)))