# Proyecto Final  - Fundamentos de programación funcional y concurrente
@Authors Carlos Alberto Camacho Castaño - 2160331
         Juan José Hernandez Arenas - 2259500
         Santiago Reyes Rodriguez - 2259738

Implementción de construccion de cadenas

En el siguiente código se presentan algoritmos de reconocimiento de subcadenas con el fin de reconocer el patrón
denominado genoma humano. El objetivo es lograr describir una pieza de DNA por medio de un
patrón o cadena de caracteres compuestos por las letras (a, g, c, t). Con las diferentes versiones, cada una mejor que la anterior, se pretenden mejorar cada vez más los tiempos de ejecución, el rendiemiento y la complejidad de las mismas, además de poder comparar su rendimiento con sus versiones paralelas y establecer si estas son viables.

Hacemos entrega de un archivo .rar donde se encuentran tres clases para el funcionamiento del proyecto (Comparar.scala, Proyecto_PFC.scala y Trie.scala).

La clase llamada "Comparar" (ruta: app/src/main/scala/Proyecto/Comparar.scala) dispone de un resumen comprarativo sobre los tiempos de ejecución de las implementaciones con su version secuencial y la solucion paralela de cada funcion establecida, haciendo comparaciones con diferentes longitudes potencias de 2 hasta 2^10. En esta clase hacemos un llamado a la funcion de comparar 
algoritmos que está en la clase Proyecto_PFC, esta funcion nos retorna el tiempo de ejecucion de las dos versiones y su aceleracion correspondiente. Para utilizar de manera adecuada esta clase, debemos de descomentar la comparacion que queremos hacer y así, el programa funciona con más eficiencia (si deseas hacer otra comparacion diferente, solo debes de comentar la que acabaste de descomentar y luego descomentar la comparacion que quieres.

La clase llamada "Proyecto_PFC" (ruta: app/src/main/scala/Proyecto/Proyecto_PFC.scala ) tiene establecidas las funciones de reconstruirCadenaIngenuo, reconstruirCadenaMejorado, reconstruirCadenaTurbo, reconstruirCadenaTurboMejorada y reconstruirCadenaTurboAcelerada, añadiendo las cinco funciones que se deben a las versiones paralelas de cada una de ellas.
También dispone de las funciones pertenece, adiciona y arbolDeSufijos, que son necesarias para el funcionamiento de reconstruirCadenaTurboAcelerada y su versión paralela. Las instrucciones para ejecutar el programa son bastantes sencillas, primero de debe dirigir a la ruta app/src/main/scala/Proyecto/Proyecto_PFC.scala, posteriormente debe de ejecutar el programa y se correran las distintas pruebas implementadas, con las diferentes versiones ubicadas en el main. Si se desea cambiar la longitud de las cadenas simplemente se edita el valor de la variable "magnitud" . 

La clase llamada "Trie" es una clase abstracta que fue proporcionada por el enunciado del proyecto, allí encontraremos funciones llamadas "raiz" y "cabezas", también el case class para el nodo y la hoja (Todo lo que se encuentra en la clase Trie es proporcionado por el enunciado del proyecto).

Se anexa en la master un archivo PDF, donde se encuentra el informe del proyecto realizado, solo debes de ingresar allí para saber más de lo desarrollado.