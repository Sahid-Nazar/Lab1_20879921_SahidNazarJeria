#lang racket
(require "main_20879921_Sahid_NazarJeria.rkt")

(display "===== CAPITALIA - PRUEBA 3 JUGADORES =====\n\n")

; 1. Creación de jugadores
(define p1 (jugador 1 "Carlos" 1500 '() 0 #f 0))
(define p2 (jugador 2 "Ana" 1500 '() 0 #f 0))
(define p3 (jugador 3 "Luis" 1500 '() 0 #f 0))

; 2. Creación de propiedades
(define prop1 (propiedad 1 "Plaza Roja" 600 2 #f 0 #f #f))
(define prop2 (propiedad 2 "Avenida Verde" 800 6 #f 0 #f #f))
(define prop3 (propiedad 3 "Paseo Azul" 1000 8 #f 0 #f #f))

; 3. Cartas de suerte y comunidad
(define chance1 (carta 1 "suerte" "Avanza a salida" 'ir-a-salida))
(define community1 (carta 2 "comunidad" "Paga impuestos" 'pagar-impuesto))

; 4. Tablero
(define tablero-vacio 
  (tablero '() (list chance1) (list community1)
           (list (cons 'salida 0) (cons 'carcel 5) (cons 'suerte 3) (cons 'comunidad 7))))

(define lista-propiedades (list (cons prop1 1) (cons prop2 4) (cons prop3 6)))
(define tablero-completo (tablero-agregar-propiedad tablero-vacio lista-propiedades))

; 5. Juego inicial
(define g0 (juego '() tablero-completo 15000 2 0 10 4 1))
(define g1 (juego-agregar-jugador g0 p1))
(define g2 (juego-agregar-jugador g1 p2))
(define g3 (juego-agregar-jugador g2 p3))

; 6. Simulación
(display "TURNO 1: Carlos\n")
(define g4 (juego-jugar-turno g3 (lanzar-dados 1 2) #t #f #f #f))
(display "TURNO 2: Ana\n")
(define g5 (juego-jugar-turno g4 (lanzar-dados 2 5) #t #f #f #f))
(display "TURNO 3: Luis\n")
(define g6 (juego-jugar-turno g5 (lanzar-dados 5 0) #t #f #f #f))
(display "TURNO 4: Carlos\n")
(define g7 (juego-jugar-turno g6 (lanzar-dados 0 4) #t #f #f #f))

; Verificación bancarrota
(display "\n¿Jugador 1 en bancarrota?: ")
(displayln (jugador-esta-en-bancarrota (get-jugador g7 0)))
(display "¿Jugador 2 en bancarrota?: ")
(displayln (jugador-esta-en-bancarrota (get-jugador g7 1)))
(display "¿Jugador 3 en bancarrota?: ")
(displayln (jugador-esta-en-bancarrota (get-jugador g7 2)))

