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
import common.{parallel, task}
import org.scalameter.{Warmer, withWarmer}

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

  def ReconstruirCadenaIngenuoPar(alfabeto: Seq[Char], longitud: Int, o: Oraculo): Seq[Char] = {
    def CadCandidatas(alfabeto: Seq[Char], longitud: Int): Seq[Seq[Char]] = {
      if (longitud == 0) {
        Seq(Seq.empty[Char])
      } else {
        val n = alfabeto.length /2
        val (alfabeto1, alfabeto2) = alfabeto.splitAt(n)
        val (p1,p2) = parallel(alfabeto1.flatMap(caracter => CadCandidatas(alfabeto, longitud - 1).map(caracter +: _)),
                               alfabeto2.flatMap(caracter => CadCandidatas(alfabeto, longitud - 1).map(caracter +: _)))
        p1 ++ p2
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
    SC.find(longitud == _.length).getOrElse(Seq())
  }

  def ReconstruirCadenaTurbo(alfabeto: Seq[Char], magnitud: Int, o: Oraculo): Seq[Char] = {
    def subcadenas_candidatas(m: Int, n: Int, SC: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (m <= magnitud) {
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


  abstract class Trie

  case class Nodo(car: Char, marcada: Boolean, hijos: List[Trie]) extends Trie

  case class Hoja(car: Char, marcada: Boolean) extends Trie

  def raiz(t: Trie): Char =
    t match {
      case Nodo(c, _, _) => c
      case Hoja(c, _) => c
    }

  def cabezas(t: Trie): Seq[Char] = {
    t match {
      case Nodo(_, _, lt) => lt.map(t => raiz(t))
      case Hoja(c, _) => Seq(c)
    }
  }

  def pertenece(s: String, t: Trie): Boolean = {
    if (s.isEmpty) {
      t match {
        case Hoja(_, marcada) => marcada
        case _ => false
      }
    } else {
      t match {
        case Nodo(c, _, hijos) =>
          hijos.exists(h => raiz(h) == s.head && pertenece(s.tail, h))
        case _ => false
      }
    }
  }

  //Funcion adicionar recibe una secuencia s y un trie t y devuelve el trie correspondiente a adicionar s a t

  def adiciona(s: String, t: Trie): Trie = {
    def adicionar(str: String, trie: Trie): Trie = {
      if (str.isEmpty) {
        trie
      } else {
        trie match {
          case Nodo(c, marcada, hijos) =>
            val (nuevoHijo, nuevosHijos) = hijos.partition(h => raiz(h) == str.head)
            val actualizadoHijo = nuevoHijo.headOption.map(h => adicionar(str.tail, h)).getOrElse(Hoja(str.head, marcada))
            Nodo(c, marcada, actualizadoHijo :: nuevosHijos)
          case Hoja(_, _) =>
            Nodo('_', false, List(adicionar(str.tail, Hoja('_', true))))
        }
      }
    }
    adicionar(s, t)
  }

  def adicionar(s: Seq[Char], t: Trie): Trie = {
    if (s.isEmpty) {
      t match {
        case Nodo(c, _, hijos) => Nodo(c, true, hijos)
        case Hoja(c, _) => Hoja(c, true)
      }
    } else {
      t match {
        case Nodo(c, marcada, hijos) => {
          val h = hijos.find(h => raiz(h) == s.head)
          if (h.isEmpty) {
            Nodo(c, marcada, hijos :+ adicionar(s, Hoja(s.head, false)))
          } else {
            Nodo(c, marcada, hijos.map(h => if (raiz(h) == s.head) adicionar(s.tail, h) else h))
          }
        }
        case Hoja(c, marcada) => Nodo(c, marcada, List(adicionar(s.tail, Hoja(s.head, false))))
      }
    }
  }


  def arbolSufijos(secuencias: Seq[String]): Trie = {
    secuencias.foldLeft(Hoja('_', false): Trie)((t, s) => adicionar(s, t))
  }




  def recontruirCadenaTurboAcelerada(magnitud: Int, o: Oraculo): Seq[Char] = {
    def reconstruirCadenaTurboAceleradaAux(m: Int, SC: Seq[Seq[Char]]): Seq[Char] = {
      if (m > magnitud) {
        SC.find(_.length == magnitud).getOrElse(Seq())    }
      else {
        val n = m * 2
        val SCk = SC.flatMap(subc => alfabeto.map(letra => subc :+ letra)).filter(o)
        //println(SCk.map(_.mkString))
        //val t = arbolSufijos(SCk.map(_.mkString))
        reconstruirCadenaTurboAceleradaAux(m + 1,SCk)
      }
    }
    val SC = reconstruirCadenaTurboAceleradaAux(1, Seq(Seq.empty[Char]))
    SC
  }




  def main(args: Array[String]): Unit = {

    //Calentamiento de la maquina virtual de java usando withWarmer
    println(
      withWarmer(new Warmer.Default) measure {
        (1 to 100000000).toArray
      }
    )

    val magnitud = 4
    val SecRandom= secuenciaaleatoria(magnitud)

    //Ejecucion de pruebas con magnitudes de 2^1 a 2^10
    for{
      i <- 1 to 10
      val magnitud = math.pow(2,i).toInt

    } yield {
      val SecRandom= secuenciaaleatoria(magnitud)
      val o: Oraculo = (s: Seq[Char]) => {
        SecRandom.containsSlice(s)
      }
      val tiempoInicioIngenuo = System.nanoTime()
      val cadena = ReconstruirCadenaIngenuo(alfabeto, magnitud, o)
      println(s" Cadena por ingenuo de tamano $magnitud: $cadena")
      val tiempoFinIngenuo = System.nanoTime()
      val tiempoIngenuo = (tiempoFinIngenuo - tiempoInicioIngenuo) / 1e6
      println(s"Tiempo de ejecucion: $tiempoIngenuo ms")

      //prueba de la ingenuo paralelo
      val tiempoInicioIngenuoPar = System.nanoTime()
      val ingenuoPar = ReconstruirCadenaIngenuoPar(alfabeto, magnitud, o)
      println(s" Cadena por ingenuo par de tamano $magnitud: $ingenuoPar")
      val tiempoFinIngenuoPar = System.nanoTime()
      val tiempoIngenuoPar = (tiempoFinIngenuoPar - tiempoInicioIngenuoPar) / 1e6
      println(s"Tiempo de ejecucion: $tiempoIngenuoPar ms\n")

      val tiempoInicioMejorado = System.nanoTime()
      val cadenaM = ReconstruirCadenaMejorado( alfabeto, magnitud, o)
      println(s" Cadena por mejorado de tamano $magnitud: $cadenaM")
      val tiempoFinalMejorado = System.nanoTime()
      val tiempoMejorado = (tiempoFinalMejorado - tiempoInicioMejorado) / 1e6
      println(s"Tiempo de ejecucion: $tiempoMejorado ms\n")

      val tiempoInicioTurbo = System.nanoTime()
      val cadenaT = ReconstruirCadenaTurbo(alfabeto, magnitud, o)
      println(s" Cadena por turbo de tamano $magnitud: $cadenaT")
      val tiempoFinTurbo = System.nanoTime()
      val tiempoTurbo = (tiempoFinTurbo - tiempoInicioTurbo) / 1e6
      println(s"Tiempo de ejecucion: $tiempoTurbo ms\n")

      val tiempoInicioTurboMejorado = System.nanoTime()
      val cadenaTM = reconstruirCadenaTurboMejorado(alfabeto, magnitud, o)
      println(s" Cadena por turbo mejorado de tamano $magnitud: $cadenaTM")
      val tiempoFinTurboMejorado = System.nanoTime()
      val tiempoTurboMejorado = (tiempoFinTurboMejorado - tiempoInicioTurboMejorado) / 1e6
      println(s"Tiempo de ejecucion: $tiempoTurboMejorado ms\n")

      val tiempoInicioTurboAcelerado = System.nanoTime()
      val turboAcelerado = recontruirCadenaTurboAcelerada(magnitud, o)
      println(s" Cadena por turbo acelerado de tamano $magnitud: $turboAcelerado")
      val tiempoFinTurboAcelerado = System.nanoTime()
      val tiempoTurboAcelerado = (tiempoFinTurboAcelerado - tiempoInicioTurboAcelerado) / 1e6
      println(s"Tiempo de ejecucion: $tiempoTurboAcelerado ms\n")
    }


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
