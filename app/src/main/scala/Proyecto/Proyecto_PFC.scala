/**
 * Proyecto Final - Programación Funcional y Concurrente
 * Autores: Carlos Alberto Camacho Castaño - 2160331
 *           Juan José Hernandez Arenas - 2259500
 *           Santiago Reyes Rodriguez - 2259738
 * Profesor: Carlos A Delgado
 */
package Proyecto
import scala.util.Random
import scala.concurrent._
import ExecutionContext.Implicits.global
import common.task

import scala.concurrent.duration.Duration
object Proyecto_PFC {


  val alfabeto = Seq('a', 'c', 'g', 't')
  type Oraculo = Seq[Char] => Boolean



  def secuenciaaleatoria(tamano: Int): String = {
    val random = new Random()
    (1 to tamano).map(_ => alfabeto(random.nextInt(alfabeto.length))).mkString
  }

  def ReconstruirCadenaIngenuo(alfabeto: Seq[Char], longitud: Int, o: Oraculo): Seq[Char] = {
    def CadCandidatas(alfabeto: Seq[Char], longitud: Int): Seq[Seq[Char]] = {
      if (longitud == 0) {
        Seq(Seq.empty[Char])
      } else {
        alfabeto.flatMap(caracter => CadCandidatas(alfabeto, longitud - 1).map(caracter +: _))
      }
    }

    val combinacionesPosibles = CadCandidatas(alfabeto, longitud)
    combinacionesPosibles.flatMap { seq =>
      if (o(seq)) seq
      else None
    }
  }

  def ReconstruirCadenaMejorado(alfabeto: Seq[Char], longitud: Int, o: Oraculo): Seq[Char] = {
    def subcadenas_candidatas(m: Int, SC: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (m <= longitud) subcadenas_candidatas(m + 1, SC.flatMap(subc => alfabeto.map(letra => subc :+ letra)).filter(o))
      else {
        SC
      }
    }

    val SC = subcadenas_candidatas(1, Seq(Seq()))
    SC.find(longitud== _.length ).getOrElse(Seq())
  }

  def ReconstruirCadenaTurbo(alfabeto: Seq[Char], magnitud: Int, o: Oraculo): Seq[Char] = {
    def subcadenas_candidatas(m: Int, n: Int, SC: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (m<=  magnitud) {
        val n = m * 2
        val SCk = SC.flatMap(subc => alfabeto.map(letra => subc :+ letra)).filter(o)
        subcadenas_candidatas(m + 1, n, SCk)
      }
      else {
        SC
      }
    }

    val SC = subcadenas_candidatas(1, 1, Seq(Seq.empty[Char]))
    SC.find(_.length == magnitud).getOrElse(Seq())

  }


  def reconstruirCadenaTurboMejorado(alfabeto: Seq[Char], magnitud: Int, o: Oraculo): Seq[Char] = {

    def subcadenas_candidatas(m: Int, n: Int, SC: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (m > magnitud) SC
      else {
        val n = m * 2
        val SCk = SC.flatMap(subc => alfabeto.map(letra => subc :+ letra)).filter(o)
        subcadenas_candidatas(m + 1, n, SCk)
      }
    }

    val SC = subcadenas_candidatas(1, 1, Seq(Seq.empty[Char]))
    SC.find(_.length == magnitud).getOrElse(Seq())

  }

/*  def reconstuirCadenaIngenuoPar(n: Int, o: Oraculo): Seq[Char] = {
    val candidatas = task(secuenciaaleatoria(n)).join()
    candidatas.find(o: Seq[Char] => Boolean).getOrElse(Seq())
  }


  def reconstruirCadenaMejoradoPar(n: Int, o: Oraculo): Seq[Char] = {
    def reconstruirRecursivo(actual: Seq[Char], longitudActual: Int): Seq[Char] = {
      if (longitudActual == n && o(actual)) {
        actual
      } else if (longitudActual < n) {
        val tareas = task(alfabeto.map(letra => task(reconstruirRecursivo(actual :+ letra, longitudActual + 1)))).join
        val siguientes = tareas.map(_.join())
        siguientes
      } else {
        Seq()
      }
    }
    reconstruirRecursivo(Seq(), 0)
  }
  */

  def main(args: Array[String]): Unit = {
    val magnitud = 4
    val SecRandom= secuenciaaleatoria(magnitud)

    val o: Oraculo = (s: Seq[Char]) => {
      SecRandom.containsSlice(s)
    }
    val tiempoInicioIngenuo = System.nanoTime()
    val cadena = ReconstruirCadenaIngenuo(alfabeto, magnitud, o)
    println(s" Cadena por ingenuo: $cadena")
    val tiempoFinIngenuo = System.nanoTime()
    val tiempoIngenuo = (tiempoFinIngenuo - tiempoInicioIngenuo) / 1e6
    println(s"Tiempo de ejecucion: $tiempoIngenuo ms")

    val tiempoInicioMejorado = System.nanoTime()
    val cadenaM = ReconstruirCadenaMejorado( alfabeto, magnitud, o)
    println(s" Cadena por mejorado: $cadenaM")
    val tiempoFinalMejorado = System.nanoTime()
    val tiempoMejorado = (tiempoFinalMejorado - tiempoInicioMejorado) / 1e6
    println(s"Tiempo de ejecucion: $tiempoMejorado ms")

    val tiempoInicioTurbo = System.nanoTime()
    val cadenaT = ReconstruirCadenaTurbo(alfabeto, magnitud, o)
    println(s" Cadena por turbo: $cadenaT")
    val tiempoFinTurbo = System.nanoTime()
    val tiempoTurbo = (tiempoFinTurbo - tiempoInicioTurbo) / 1e6
    println(s"Tiempo de ejecucion: $tiempoTurbo ms")


    val tiempoInicioTurboMejorado = System.nanoTime()
    val cadenaTM = reconstruirCadenaTurboMejorado(alfabeto, magnitud, o)
    println(s" Cadena por turbo mejorado: $cadenaTM")
    val tiempoFinTurboMejorado = System.nanoTime()
    val tiempoTurboMejorado = (tiempoFinTurboMejorado - tiempoInicioTurboMejorado) / 1e6
    println(s"Tiempo de ejecucion: $tiempoTurboMejorado ms")
/*
    val tiempoInicioPar = System.nanoTime()
    val ingenuoPar = reconstuirCadenaIngenuoPar(magnitud, o)
    println(s" Cadena por ingenuo par: $ingenuoPar")
    val tiempoFinPar = System.nanoTime()
    val tiempoPar = (tiempoFinPar - tiempoInicioPar) / 1e6
    println(s"Tiempo de ejecucion: $tiempoPar ms")

    val tiempoInicioMejoradoPar = System.nanoTime()
    val mejoradoPar = reconstruirCadenaMejoradoPar(magnitud, o)
    println(s" Cadena por mejorado par: $mejoradoPar")
    val tiempoFinMejoradoPar = System.nanoTime()
    val tiempoMejoradoPar = (tiempoFinMejoradoPar - tiempoInicioMejoradoPar) / 1e6
    println(s"Tiempo de ejecucion: $tiempoMejoradoPar ms")
*/
  }

}
