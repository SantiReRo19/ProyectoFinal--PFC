package Proyecto

import Proyecto.Proyecto_PFC.{Benchmark, Oraculo, reconstruirCadenaIngenuo, reconstruirCadenaIngenuoPar, reconstruirCadenaMejorado, reconstruirCadenaMejoradoPar, reconstruirCadenaTurbo, reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboMejorado, reconstruirCadenaTurboMejoradoPar, reconstruirCadenaTurboPar, secuenciaaleatoria}

object Comparar {/*Parte para comparar algoritmos y su aceleracion, se recomienda
poner como comentario las funciones que no van a ser comparadas, para unmejor rendimiento */
  val alfabeto = Seq('a', 'c', 'g', 't')
  type Oraculo = Seq[Char] => Boolean


  def main(args: Array[String]): Unit = {

    print("\n")
    print("Comparacion ingenuo e ingenuo par: \n")
    for {
      i <- 1 to 10
      m: Oraculo = (s: Seq[Char]) => {
        secuenciaaleatoria(math.pow(2, i).toInt).containsSlice(s)
      }
    } yield (println("Tamano de la secuencia: " + math.pow(2, i).toInt +
      "\nTiempo secuencial, Paralelo, aceleracion: " + Benchmark.compararAlgoritmos(reconstruirCadenaIngenuo, reconstruirCadenaIngenuoPar)(alfabeto, math.pow(2, i).toInt, m)))


/*
    print("\n")
    print("Comparacion mejorado y mejorado par: \n")
    for {
      i <- 1 to 10
      m: Oraculo = (s: Seq[Char]) => {
        secuenciaaleatoria(math.pow(2, i).toInt).containsSlice(s)
      }
    } yield (println("Tamano de la secuencia: " + math.pow(2, i).toInt +
      "\nTiempo secuencial, Paralelo, aceleracion: " + Benchmark.compararAlgoritmos(reconstruirCadenaMejorado, reconstruirCadenaMejoradoPar)(alfabeto, math.pow(2, i).toInt, m)))
*/

/*

    print("\n")
    print("Comparacion turbo y turbo par: \n")
    for {
      i <- 1 to 10
      m: Oraculo = (s: Seq[Char]) => {
        secuenciaaleatoria(math.pow(2, i).toInt).containsSlice(s)
      }
    } yield (println("Tamano de la secuencia: " + math.pow(2, i).toInt +
      "\nTiempo secuencial, Paralelo, aceleracion: " + Benchmark.compararAlgoritmos(reconstruirCadenaTurbo, reconstruirCadenaTurboPar)(alfabeto, math.pow(2, i).toInt, m)))

*/

/*
    print("\n")
    print("Comparacion turbo mejorado y turbo mejorado par: \n")
    for {
      i <- 1 to 10
      m: Oraculo = (s: Seq[Char]) => {
        secuenciaaleatoria(math.pow(2, i).toInt).containsSlice(s)
      }
    } yield (println("Tamano de la secuencia: " + math.pow(2, i).toInt +
      "\nTiempo secuencial, Paralelo, aceleracion: " + Benchmark.compararAlgoritmos(reconstruirCadenaTurboMejorado, reconstruirCadenaTurboMejoradoPar)(alfabeto, math.pow(2, i).toInt, m)))

*/
/*


    print("\n")
    print("Comparacion turbo acelerada y turbo acelerada par: \n")
    for {
      i <- 1 to 10
      m: Oraculo = (s: Seq[Char]) => {
        secuenciaaleatoria(math.pow(2, i).toInt).containsSlice(s)
      }
    } yield (println("Tamano de la secuencia: " + math.pow(2, i).toInt +
      "\nTiempo secuencial, Paralelo, aceleracion: " + Benchmark.compararAlgoritmos(reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar)(alfabeto, math.pow(2, i).toInt, m)))
*/
  }

}
