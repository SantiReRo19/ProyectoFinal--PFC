/**
 * Proyecto - Programación Funcional y Concurrente
 * Autores: Carlos Alberto Camacho Castaño - 2160331
 *           Juan José Hernandez Arenas - 2259500
 *           Santiago Reyes Rodriguez - 2259738
 *           Carlos Alberto Camacho Castaño -2160331
 * Profesor: Carlos A Delgado
 */


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
  abstract class Trie
  case class Nodo(car: Char, marcada: Boolean, hijos: List[Trie]) extends Trie

  case class Hoja(car: Char, marcada: Boolean) extends Trie

  def raiz(t: Trie): Char =
    t match {
      case Nodo(c, _, _) => c
      case Hoja(c, _) => c
    }

  def cabezas(t: Trie): Seq[Char] =
    t match {
      case Nodo(_, _, lt) => lt.map(t => raiz(t))
      case Hoja(c, _) => Seq(c)
    }

  def pertenece(s: Seq[Char], t: Trie): Boolean = {
    s match {
      case Seq() => true
      case c +: cs =>
        t match {
          case Nodo(_, _, hijos) =>
            hijos.exists(h => raiz(h) == c && pertenece(cs, h))
          case Hoja(_, _) => false
        }
    }
  }

  def adicionar(s: Seq[Char], t: Trie): Trie = {
    s match {
      case Seq() => t
      case c +: cs =>
        t match {
          case Nodo(car, marcada, hijos) =>
            val nuevoHijo = hijos.find(h => raiz(h) == c) match {
              case Some(hijo) => adicionar(cs, hijo)
              case None => Nodo(c, false, List()) // Crear un nuevo nodo si no existe uno con el caracter actual
            }
            Nodo(car, marcada, nuevoHijo +: hijos.filter(h => raiz(h) != c))
          case Hoja(_, _) => t
        }
    }
  }

  def arbolDeSufijos(ss: Seq[Seq[Char]]): Trie = {
    def construirTrie(sufijos: Seq[Seq[Char]], t: Trie): Trie = {
      sufijos match {
        case Seq() => t
        case s +: rest =>
          val nuevoTrie = adicionar(s.reverse, t)
          construirTrie(rest, nuevoTrie)
      }
    }

    construirTrie(ss, Nodo(' ', false, List.empty[Trie]))
  }

  def reconstruirCadenaTurboAcelerada(alfabeto:Seq[Char],n: Int, o: Oraculo): Seq[Char] = {
    val trie = arbolDeSufijos((1 to n).map(_ => secuenciaaleatoria(n)).toList.map(_.toSeq))

    def reconstruirRecursivo(tamano: Int, oraculo: Oraculo, t: Trie): Seq[Char] = {
      if (tamano == 1) {
        val caracter = alfabeto.find(c => oraculo(Seq(c))).getOrElse(alfabeto.head)
        Seq(caracter)
      } else {
        val mitad = tamano / 2
        val primeraMitad = reconstruirRecursivo(mitad, oraculo, adicionar(Seq('a'), t))
        val segundaMitad = reconstruirRecursivo(mitad, oraculo, adicionar(Seq('c'), t))
        if (oraculo(primeraMitad ++ segundaMitad)) {
          primeraMitad ++ segundaMitad
        } else {
          segundaMitad ++ primeraMitad
        }
      }
    }

    reconstruirRecursivo(n, o, trie)
  }


  def main(args: Array[String]): Unit = {
    val secuencia1 = Seq('a', 'c', 'a', 'c', 'g', 't')

    val o: Oraculo = (s: Seq[Char]) => {
      secuencia1.containsSlice(s)
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


    val tiempoInicioTurboMejorado = System.nanoTime()
    val cadenaTM = reconstruirCadenaTurboMejorado(alfabeto, 6, o)
    println(s" Cadena por turbo: $cadenaTM")
    val tiempoFinTurboMejorado = System.nanoTime()
    val tiempoTurboMejorado = (tiempoFinTurboMejorado - tiempoInicioTurboMejorado) / 1e6
    println(s"Tiempo de ejecucion: $tiempoTurboMejorado ms")

    val tiempoInicioTurboAcelerada = System.nanoTime()
    val cadenaTurboAcelerada = reconstruirCadenaTurboAcelerada(alfabeto,6, o)
    println(s" Cadena por turbo acelerado: $cadenaTurboAcelerada")
    val tiempoFinTurboAcelerada = System.nanoTime()
    val tiempoTurboAcelerada = (tiempoFinTurboAcelerada - tiempoInicioTurboAcelerada) / 1e6
    println(s"Tiempo de ejecución: $tiempoTurboAcelerada ms")





  }

}
