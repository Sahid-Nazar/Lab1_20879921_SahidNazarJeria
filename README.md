# Proyecto: Capitalia - Juego de Mesa

## Descripción

Este proyecto implementa el juego **Capitalia**, inspirado en **Monopoly**, utilizando el lenguaje de programación **Racket**.  
El proyecto está estructurado a través de varios **TDA** (Tipos Abstractos de Datos) que representan los elementos principales del juego: jugadores, propiedades, tablero y gestión del juego en general.

El objetivo es simular una partida de mesa de forma programada, respetando las reglas básicas de movimiento, compra de propiedades, pago de rentas y manejo de cárcel.

## Estructura del Proyecto

El proyecto está compuesto por los siguientes archivos:

- `player_20879921_NazarJeria.rkt`: Implementación del **TDA Jugador**.
- `property_20879921_NazarJeria.rkt`: Implementación del **TDA Propiedad**.
- `board_20879921_NazarJeria.rkt`: Implementación del **TDA Tablero**.
- `card_20879921_NazarJeria.rkt`: Implementación del **TDA Carta** (cartas de suerte y comunidad).
- `game_20879921_NazarJeria.rkt`: Implementación del **TDA Juego**.
- `main_20879921_Sahid_NazarJeria.rkt`: Archivo principal de requerimientos para unificar los TDAs.
- `script_base_20879921_Sahid_NazarJeria.rkt`: **Script de simulación principal** que realiza la ejecución de turnos y controla el flujo del juego.
- `script1_20879921_NazarJeria.rkt`: Script de prueba con 2 jugadores.
- `script2_20879921_NazarJeria.rkt`: Script de prueba con 3 jugadores.
- `Informe_20879921_NazarJeria.pdf`: Informe del proyecto.
- `AUTOEVALUACION_20879921_NazarJeria.txt`: Autoevaluación del trabajo realizado.

## Instrucciones de Uso

1. Abrir DrRacket.
2. Cargar el archivo `script_base_20879921_Sahid_NazarJeria.rkt` o cualquiera de los scripts personalizados.
3. Ejecutar el archivo presionando el botón "Run".
4. Se visualizarán por consola los resultados de la simulación de turnos de juego.

### Requisitos:

- Tener instalado **Racket** (versión recomendada: 8.9 o superior).  
  Puedes descargarlo desde (https://racket-lang.org/).

## Observaciones

- Las simulaciones cubren casos de compra de propiedades, movimiento, pago de renta, estadía en la cárcel, uso de cartas y bancarrota.
- Todas las funciones fueron testeadas en scripts personalizados.


## Repositorio Público GitHub

- Puedes acceder al repositorio del proyecto en el siguiente enlace:
  https://github.com/Sahid-Nazar/Lab1_20879921_SahidNazarJeria