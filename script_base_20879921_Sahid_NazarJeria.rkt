#lang racket
(require "main_20879921_Sahid_NazarJeria.rkt")

; 1. Creación de jugadores
(define p1 (jugador 1 "Carlos" 1500 '() 0 #f 0))
(define p2 (jugador 2 "Ana" 1500 '() 0 #f 0))

; 2. Creación de propiedades para el juego
(define prop1 (propiedad 1 "Paseo Mediterráneo" 600 2 #f 0 #f #f))
(define prop2 (propiedad 2 "Avenida Báltica" 600 4 #f 0 #f #f))
(define prop3 (propiedad 3 "Avenida Oriental" 100 6 #f 0 #f #f))
(define prop4 (propiedad 4 "Avenida Vermont" 100 6 #f 0 #f #f))
(define prop5 (propiedad 5 "Avenida Connecticut" 120 8 #f 0 #f #f))
(define prop6 (propiedad 6 "Plaza San Carlos" 900 10 #f 0 #f #f))
(define prop7 (propiedad 7 "Avenida St. James" 180 14 #f 0 #f #f))
(define prop8 (propiedad 8 "Avenida Tennessee" 900 14 #f 0 #f #f))

; 3. Creación de cartas de suerte y comunidad
(define chance1 (carta 1 "suerte" "Avance hasta la casilla de salida" 'ir-a-salida))
(define chance2 (carta 2 "suerte" "Vaya a la cárcel" 'ir-a-carcel))
(define chance3 (carta 3 "suerte" "El banco le paga $50" 'banco-paga))

(define community1 (carta 4 "comunidad" "Pague impuestos por $100" 'pagar-impuesto))
(define community2 (carta 5 "comunidad" "Es su cumpleaños, reciba $10 de cada jugador" 'cumpleanos))
(define community3 (carta 6 "comunidad" "Salga de la cárcel gratis" 'salir-carcel))

; 4. Creación del tablero
(define tablero-vacio 
  (tablero '()
          (list chance1 chance2 chance3)
          (list community1 community2 community3)
          (list
           (cons 'salida 0)
           (cons 'carcel 2)
           (cons 'carcel 5)
           (cons 'suerte 7)
           (cons 'suerte 12)
           (cons 'comunidad 10))))

(define lista-propiedades
  (list (cons prop1 1) (cons prop2 3) (cons prop3 6) (cons prop4 8)
        (cons prop5 9) (cons prop6 11) (cons prop7 13) (cons prop8 14)))

(define tablero-completo (tablero-agregar-propiedad tablero-vacio lista-propiedades))

; 5. Creación del juego
(define g0 (juego '() tablero-completo 20000 2 0 10 4 1))

; 6. Agregar jugadores al juego
(define g1 (juego-agregar-jugador g0 p1))
(define g2 (juego-agregar-jugador g1 p2))

; 7. Jugar (inicio de simulación)
(display "===== CAPITALIA =====\n\n")

; Turno 1: Carlos
(display "TURNO 1: Carlos\n")
(define g3 (juego-jugar-turno g2 (lanzar-dados 3 4) #t #f #f #f))
(display "Turno 1 completado.\n")

; Turno 2: Ana
(display "TURNO 2: Ana\n")
(define g4 (juego-jugar-turno g3 (lanzar-dados 2 5) #t #f #f #f))
(display "Turno 2 completado.\n")

; Turno 3: Carlos
(display "TURNO 3: Carlos\n")
(define g5 (juego-jugar-turno g4 (lanzar-dados 5 0) #t #f #f #f))
(display "Turno 3 completado.\n")

; Turno 4: Ana
(display "TURNO 4: Ana\n")
(define g6 (juego-jugar-turno g5 (lanzar-dados 3 4) #f #f #t #f))
(display "Turno 4 completado.\n")

; Turno 5: Carlos
(display "TURNO 5: Carlos\n")
(define g7 (juego-jugar-turno g6 (lanzar-dados 1 2) #t #f #f #f))
(display "Turno 5 completado.\n")

; Verificación de bancarrota
(display "\n¿Jugador 1 en bancarrota?: ")
(displayln (jugador-esta-en-bancarrota (get-jugador g7 0)))
(display "¿Jugador 2 en bancarrota?: ")
(displayln (jugador-esta-en-bancarrota (get-jugador g7 1)))

; Fin del script ejecucion




