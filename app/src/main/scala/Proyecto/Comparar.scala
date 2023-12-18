package Proyecto

import Proyecto.Proyecto_PFC.{Benchmark, Oraculo, reconstruirCadenaIngenuo, reconstruirCadenaIngenuoPar, reconstruirCadenaMejorado, reconstruirCadenaMejoradoPar, reconstruirCadenaTurbo, reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar, reconstruirCadenaTurboMejorado, reconstruirCadenaTurboMejoradoPar, reconstruirCadenaTurboPar, secuenciaaleatoria}
import org.scalameter.{Warmer, withWarmer}

object Comparar {

  val alfabeto = Seq('a', 'c', 'g', 't')
  type Oraculo = Seq[Char] => Boolean

  def comparaciones(funcion1: (Seq[Char], Int, Oraculo) => Seq[Char], funcion2: (Seq[Char], Int, Oraculo) => Seq[Char], name: String, Umbral: Int): Unit = {
    println(name)
    for {
      i <- 1 to Umbral
      m: Oraculo = (s: Seq[Char]) => {
        secuenciaaleatoria(math.pow(2, i).toInt).containsSlice(s)
      }
    } yield (
      println("Tamano de la secuencia: " + math.pow(2, i).toInt +
        "\nTiempo secuencial, Paralelo, aceleracion: " + Benchmark.compararAlgoritmos(funcion1, funcion2)(alfabeto, math.pow(2, i).toInt, m)))
  }


  def main(args: Array[String]): Unit = {

    //calentar JVM con withWarmer
    println(
      withWarmer(new Warmer.Default) measure {
        (1 to 100000000).toArray
      }
    )

    comparaciones(reconstruirCadenaIngenuo, reconstruirCadenaIngenuoPar, "\nCOMPARACION INGENUO E INGENIO PAR: \n", 3)
    comparaciones(reconstruirCadenaMejorado, reconstruirCadenaMejoradoPar, "\nCOMPARACION MEJORADO Y MEJORADO PAR: \n", 10)
    comparaciones(reconstruirCadenaTurbo, reconstruirCadenaTurboPar, "\nCOMPARACION TURBO Y TURBO PAR: \n", 10)
    comparaciones(reconstruirCadenaTurboMejorado, reconstruirCadenaTurboMejoradoPar, "\nCOMPARACION TURBO MEJORADO Y TURBO MEJORADO PAR: \n", 10)

    println("\nCOMPARACION TURBO ACELERADO Y TURBO ACELERADO PAR: \n")
    for {
      i <- 1 to 10
    } yield {
      val magnitud = math.pow(2, i).toInt
      val SecRandom = secuenciaaleatoria(magnitud)
      val o: Oraculo = (s: Seq[Char]) => {
        SecRandom.containsSlice(s)
      }

      val tiempoInicioTurboAcelerado = System.nanoTime()
      val turboAcelerado = reconstruirCadenaTurboAcelerada(magnitud, o)
      val tiempoFinTurboAcelerado = System.nanoTime()
      val tiempoTurboAcelerado = (tiempoFinTurboAcelerado - tiempoInicioTurboAcelerado) / 1e6
      val tiempoInicioTurboAceleradoPar = System.nanoTime()
      val turboAceleradoPar = reconstruirCadenaTurboAceleradaPar(magnitud, o)
      val tiempoFinTurboAceleradoPar = System.nanoTime()
      val tiempoTurboAceleradoPar = (tiempoFinTurboAceleradoPar - tiempoInicioTurboAceleradoPar) / 1e6
      println("Tamano de la secuencia: " + magnitud)
      println("Tiempo Turbo Acelerado Secuencial: " + tiempoTurboAcelerado + " ms  " + tiempoTurboAceleradoPar + " ms")
    }


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

    print("\n")
    print("Comparacion turbo y turbo par: \n")
    for {
      i <- 1 to 10
      m: Oraculo = (s: Seq[Char]) => {
        secuenciaaleatoria(math.pow(2, i).toInt).containsSlice(s)
      }
    } yield (println("Tamano de la secuencia: " + math.pow(2, i).toInt +
      "\nTiempo secuencial, Paralelo, aceleracion: " + Benchmark.compararAlgoritmos(reconstruirCadenaTurbo, reconstruirCadenaTurboPar)(alfabeto, math.pow(2, i).toInt, m)))

    print("\n")
    print("Comparacion turbo mejorado y turbo mejorado par: \n")
    for {
      i <- 1 to 10
      m: Oraculo = (s: Seq[Char]) => {
        secuenciaaleatoria(math.pow(2, i).toInt).containsSlice(s)
      }
    } yield (println("Tamano de la secuencia: " + math.pow(2, i).toInt +
      "\nTiempo secuencial, Paralelo, aceleracion: " + Benchmark.compararAlgoritmos(reconstruirCadenaTurboMejorado, reconstruirCadenaTurboMejoradoPar)(alfabeto, math.pow(2, i).toInt, m)))
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


     */
}}
