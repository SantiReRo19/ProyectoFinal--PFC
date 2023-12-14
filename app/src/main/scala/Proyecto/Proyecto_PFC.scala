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

  def ReconstruirCadenaIngenuo( n: Int, o: Oraculo): Seq[Char] = {
    val k=0;
    def CadCandidatas(alfabeto: Seq[Char], k: Int): Seq[Seq[Char]] = {
      if (n==k) {
        Seq(Seq.empty[Char])
      } else {
        alfabeto.flatMap(caracter => CadCandidatas(alfabeto, k+1).map(caracter +: _))
      }
    }

    val combinacionesPosibles = CadCandidatas(alfabeto, k)
    combinacionesPosibles.flatMap { seq =>
      if (o(seq)) seq
      else None
    }
  }

  def ReconstruirCadenaMejorado( n: Int, o: Oraculo): Seq[Char] = {

    def subcaden_candidatas(n: Int, SC: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (n==1) SC
      else {
        val sck = SC.flatMap(elementos_sc => alfabeto.map(letra => elementos_sc :+ letra)).filter(o)
        subcaden_candidatas(n-1, sck)
      }
    }

    val SC = subcaden_candidatas(n, Seq(Seq()))
    SC.head
  }

  def ReconstruirCadenaTurbo( n: Int, o: Oraculo): Seq[Char] = {
    def subcaden_candidatas(m: Int, SC: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (m >= n) SC
      else {
        val SCk = SC.flatMap { c1 =>
          SC.flatMap { c2 =>
            Seq(c1 ++ c2)
          }
        }
        val SCkFiltrado = SCk.filter(o)
        subcaden_candidatas(m * 2, SCkFiltrado)
      }
    }
    val Alfabet = alfabeto.map(Seq(_))
    val SC = subcaden_candidatas(1, Alfabet)
    SC.head

  }


  def reconstruirCadenaTurboMejorado(alfabeto:Seq[Char], n: Int, o: Oraculo): Seq[Char] = {

    def subcadenas_candidatas(m: Int, n: Int, SC: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (m > n) SC
      else {
        val n = m * 2
        val SCk = SC.flatMap(subc => alfabeto.map(letra => subc :+ letra)).filter(o)
        subcadenas_candidatas(m + 1, n, SCk)
      }
    }

    val SC = subcadenas_candidatas(1, 1, Seq(Seq.empty[Char]))
    SC.find(_.length == n).getOrElse(Seq())

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
    val magnitud = 8
    val SecRandom= secuenciaaleatoria(magnitud)

    val o: Oraculo = (s: Seq[Char]) => {
      SecRandom.containsSlice(s)
    }
    val tiempoInicioIngenuo = System.nanoTime()
    val cadena = ReconstruirCadenaIngenuo( magnitud, o)
    println(s" Cadena por ingenuo: $cadena")
    val tiempoFinIngenuo = System.nanoTime()
    val tiempoIngenuo = (tiempoFinIngenuo - tiempoInicioIngenuo) / 1e6
    println(s"Tiempo de ejecucion: $tiempoIngenuo ms")

    val tiempoInicioMejorado = System.nanoTime()
    val cadenaM = ReconstruirCadenaMejorado( magnitud, o)
    println(s" Cadena por mejorado: $cadenaM")
    val tiempoFinalMejorado = System.nanoTime()
    val tiempoMejorado = (tiempoFinalMejorado - tiempoInicioMejorado) / 1e6
    println(s"Tiempo de ejecucion: $tiempoMejorado ms")

    val tiempoInicioTurbo = System.nanoTime()
    val cadenaT = ReconstruirCadenaTurbo( magnitud, o)
    println(s" Cadena por turbo: $cadenaT")
    val tiempoFinTurbo = System.nanoTime()
    val tiempoTurbo = (tiempoFinTurbo - tiempoInicioTurbo) / 1e6
    println(s"Tiempo de ejecucion: $tiempoTurbo ms")

/*
    val tiempoInicioTurboMejorado = System.nanoTime()
    val cadenaTM = reconstruirCadenaTurboMejorado(alfabeto, magnitud, o)
    println(s" Cadena por turbo mejorado: $cadenaTM")
    val tiempoFinTurboMejorado = System.nanoTime()
    val tiempoTurboMejorado = (tiempoFinTurboMejorado - tiempoInicioTurboMejorado) / 1e6
    println(s"Tiempo de ejecucion: $tiempoTurboMejorado ms")

    val tiempoInicioTurboAcelerada = System.nanoTime()
    val cadenaTurboAcelerada = reconstruirCadenaTurboAcelerada(alfabeto,magnitud, o)
    println(s" Cadena por turbo acelerado: $cadenaTurboAcelerada")
    val tiempoFinTurboAcelerada = System.nanoTime()
    val tiempoTurboAcelerada = (tiempoFinTurboAcelerada - tiempoInicioTurboAcelerada) / 1e6
    println(s"Tiempo de ejecución: $tiempoTurboAcelerada ms")*/





  }

}
