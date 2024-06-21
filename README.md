# Q*Bert

## Repositorio con el trabajo final de la materia Programación Avanzada: Q*Bert (1983).

 Desarrollado por: Brugevin Lucas y Gomez Fernando.

## Consideraciones de la implementación:

Se han implementado los siguientes scripts:


Dentro del directorio `Qbert\src\QbertLibraries\`
- `DataStructureQbert.fs`: Contiene las estructuras de datos utilizadas en el juego. 

    - El tablero se trata de una matriz cuadriculada de 9x9, de un tipo de dato llamado `Cell`, que contiene la característica si una casilla ha sido visitada o no. Las casillas funcionales se tratan de la parte superior izquierda de la matriz. 
    - Conjunto de criaturas: Qbert, RedBall, PurpleBall, Coily, Sam, GreenBall, Ugg, WrongWay. Para ver las características particulares de cada criatura, mirar el archivo.

- `FunctionsQbert.fs`: Contiene las funciones que se utilizan en el juego. 

    - Funciones de movimiento de las criaturas.
    - Funciones de cambio de estado de las criaturas.
    - Funciones de verificación de colisiones.
    - Funciones de actualización del tablero.
    - Funciones de fin de juego.
    - Funciones de victoria.
    - Entre otras.

    Para cada función se ha documentado su propósito, parámetros de entrada y salida, y ejemplos de uso.

Dentro del directorio `Qbert\Test\QbertTest\`
- `UnitTest.fs`: Contiene los test de las funciones implementadas en `FunctionsQbert.fs`. 

Todas estas funcionalidades están condensadas e integradas dentro del directorio `Qbert\`, en la solución `Qbert.sln`.