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

  }

}
