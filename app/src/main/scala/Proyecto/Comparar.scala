package Proyecto

import Proyecto.Algoritmos.{Benchmark, Oraculo, reconstruirCadenaIngenuo, reconstruirCadenaIngenuoPar, reconstruirCadenaMejorado, reconstruirCadenaMejoradoPar, reconstruirCadenaTurbo, reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar, reconstruirCadenaTurboMejorado, reconstruirCadenaTurboMejoradoPar, reconstruirCadenaTurboPar, secuenciaaleatoria}
import org.scalameter.{Warmer, withWarmer}

object Comparar {

  val alfabeto = Seq('a', 'c', 'g', 't')
  type Oraculo = Seq[Char] => Boolean

  def comparaciones(funcion1: (Int, Oraculo) => Seq[Char], funcion2: (Int, Oraculo) => Seq[Char], name: String, Umbral: Int): Unit = {
    println(name)
    for {
      i <- 1 to Umbral
      m: Oraculo = (s: Seq[Char]) => {
        secuenciaaleatoria(math.pow(2, i).toInt).containsSlice(s)
      }
    } yield (
      println("Tamano de la secuencia: " + math.pow(2, i).toInt +
        "\nTiempo secuencial, Paralelo, aceleracion: " + Benchmark.compararAlgoritmos(funcion1, funcion2)((math.pow(2, i).toInt), m)))
  }

  def main(args: Array[String]): Unit = {

    //calentar JVM con withWarmer
    println(
      withWarmer(new Warmer.Default) measure {
        (1 to 100000000).toArray
      }
    )

    comparaciones(reconstruirCadenaIngenuo, reconstruirCadenaIngenuoPar, "\nCOMPARACION INGENUO E INGENIO PAR: \n", 3)
    comparaciones(reconstruirCadenaMejorado, reconstruirCadenaMejoradoPar, "\nCOMPARACION MEJORADO Y MEJORADO PAR: \n", 5)
    comparaciones(reconstruirCadenaTurbo, reconstruirCadenaTurboPar, "\nCOMPARACION TURBO Y TURBO PAR: \n", 5)
    comparaciones(reconstruirCadenaTurboMejorado, reconstruirCadenaTurboMejoradoPar, "\nCOMPARACION TURBO MEJORADO Y TURBO MEJORADO PAR: \n", 5)
    //comparaciones(reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar, "\nCOMPARACION TURBO ACELERADO Y TURBO ACELERADO PAR: \n", 5)

  }
}