/**
 * Proyecto - Programación Funcional y Concurrente
 * Autores: Carlos Alberto Camacho Castaño - 2160331
 *           Juan José Hernandez Arenas - 2259500
 *           Santiago Reyes Rodriguez - 2259738
 *           Carlos Alberto Camacho Castaño -2160331
 * Profesor: Carlos A Delgado
 */
package Proyecto
import scala.util.Random

object Proyecto_PFC {


    val alfabeto = Seq('a', 'c', 'g', 't')
    type Oraculo = Seq[Char] => Boolean



    def secuenciaaleatoria(tamano: Int): String = {
      val random = new Random()
      (1 to tamano).map(_ => alfabeto(random.nextInt(alfabeto.length))).mkString
    }

  def ReconstruirCadenaIngenuo(alfabeto: Seq[Char], longitud: Int, o: Oraculo): Seq[Char] = {
    def cadenas_candidatas(alfabeto: Seq[Char], longitud: Int): Seq[Seq[Char]] = {
      if (longitud == 0) {
        Seq(Seq.empty[Char])
      } else {
        alfabeto.flatMap(caracter => cadenas_candidatas(alfabeto, longitud - 1).map(caracter +: _))
      }
    }

    val combinacionesPosibles = cadenas_candidatas(alfabeto, longitud)
    combinacionesPosibles.flatMap { seq =>
      if (o(seq)) seq
      else None
    }
  }

  def ReconstruirCadenaMejorado(alfabeto: Seq[Char], longitud: Int, o: Oraculo): Seq[Char] = {
    def subcaden_candidatas(k: Int, SC: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (k > longitud) SC
      else {
        subcaden_candidatas(k + 1, SC.flatMap(subc => alfabeto.map(letra => subc :+ letra)).filter(o))
      }
    }

    val SC = subcaden_candidatas(1, Seq(Seq()))
    SC.find(longitud== _.length ).getOrElse(Seq())
  }

  def ReconstruirCadenaTurbo(alfabeto: Seq[Char], long: Int, o: Oraculo): Seq[Char] = {
    def subcaden_candidataspar(k: Int, n: Int, SC: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (k > long) SC
      else {
        val n = k * 2
        val SCk = SC.flatMap(subc => alfabeto.map(letra => subc :+ letra)).filter(o)
        subcaden_candidataspar(k + 1, n, SCk)
      }
    }

    val SC = subcaden_candidataspar(1, 1, Seq(Seq.empty[Char]))
    SC.find(_.length == long).getOrElse(Seq())

  }

    def main(args: Array[String]): Unit = {
      val secuencia = Seq('a', 'c', 'a', 'c', 'g', 't')

      val o: Oraculo = (s: Seq[Char]) => {
        secuencia.containsSlice(s)
      }
      val tiempoInicioIngenuo = System.nanoTime()
      val cadena = ReconstruirCadenaIngenuo(alfabeto, 6, o)
      println(s" Cadena por ingenuo: $cadena")
      val tiempoFinIngenuo = System.nanoTime()
      val tiempoIngenuo = (tiempoFinIngenuo - tiempoInicioIngenuo) / 1e6
      println(s"Tiempo de ejecucion: $tiempoIngenuo ms")

      val tiempoInicioMejorado = System.nanoTime()
      val cadenaM = ReconstruirCadenaMejorado(alfabeto, 6, o)
      println(s" Cadena por mejorado: $cadenaM")
      val tiempoFinalMejorado = System.nanoTime()
      val tiempoMejorado = (tiempoFinalMejorado - tiempoInicioMejorado) / 1e6
      println(s"Tiempo de ejecucion: $tiempoMejorado ms")

      val tiempoInicioTurbo = System.nanoTime()
      val cadenaT = ReconstruirCadenaTurbo(alfabeto, 6, o)
      println(s" Cadena por turbo: $cadenaT")
      val tiempoFinTurbo = System.nanoTime()
      val tiempoTurbo = (tiempoFinTurbo - tiempoInicioTurbo) / 1e6
      println(s"Tiempo de ejecucion: $tiempoTurbo ms")


    }

  }
