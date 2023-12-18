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
import java.util.concurrent.{ForkJoinPool, RecursiveTask}

import scala.concurrent.duration.Duration
object Proyecto_PFC {


  val alfabeto = Seq('a', 'c', 'g', 't')
  type Oraculo = Seq[Char] => Boolean


  def secuenciaaleatoria(tamano: Int): String = {
    val random = new Random()
    (1 to tamano).map(_ => alfabeto(random.nextInt(alfabeto.length))).mkString
  }

  def reconstruirCadenaIngenuo(alfabeto: Seq[Char], longitud: Int, o: Oraculo): Seq[Char] = {
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

  def reconstruirCadenaIngenuoPar(alfabeto: Seq[Char], longitud: Int, o: Oraculo): Seq[Char] = {
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

  def reconstruirCadenaMejorado(alfabeto: Seq[Char], longitud: Int, o: Oraculo): Seq[Char] = {
    def subcadenas_candidatas(m: Int, SC: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (m <= longitud) subcadenas_candidatas(m + 1, SC.flatMap(subc => alfabeto.map(letra => subc :+ letra)).filter(o))
      else {
        SC
      }
    }
    val SC = subcadenas_candidatas(1, Seq(Seq()))
    SC.find(longitud == _.length).getOrElse(Seq())
  }

  def reconstruirCadenaTurbo(alfabeto: Seq[Char], magnitud: Int, o: Oraculo): Seq[Char] = {
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

  def reconstruirCadenaTurboMejoradoPar(alfabeto: Seq[Char], magnitud: Int, oraculo: Oraculo): Seq[Char] = {
    class SubcadenasTask(m: Int, n: Int, SC: Seq[Seq[Char]], alfabeto: Seq[Char], magnitud: Int, oraculo: Oraculo) extends RecursiveTask[Seq[Seq[Char]]] {
      override def compute(): Seq[Seq[Char]] = {
        if (m > magnitud) SC
        else {
          val n = m * 2
          val SCk = SC.flatMap(subc => alfabeto.map(letra => subc :+ letra).filter(oraculo))
          val tasks = SCk.map(subc => new SubcadenasTask(m + 1, n, Seq(subc), alfabeto, magnitud, oraculo))
          tasks.foreach(_.fork())
          val results = tasks.map(_.join())
          results.flatten
        }
      }
    }
    val fjPool = new ForkJoinPool()
    val task = new SubcadenasTask(1, 1, Seq(Seq.empty[Char]), alfabeto, magnitud, oraculo)
    val result = fjPool.invoke(task)
    result.find(_.length == magnitud).getOrElse(Seq())
  }

  def reconstruirCadenaTurboPar(alfabeto: Seq[Char], magnitud: Int, oraculo: Oraculo): Seq[Char] = {
    class Subcadenas(m: Int, n: Int, SC: Seq[Seq[Char]], alfabeto: Seq[Char], magnitud: Int, oraculo: Oraculo) extends RecursiveTask[Seq[Seq[Char]]] {
      override def compute(): Seq[Seq[Char]] = {
        if (m <= magnitud) {
          val n = m * 2
          val SCk = SC.flatMap(subc => alfabeto.map(letra => subc :+ letra).filter(oraculo))
          val tasks = SCk.map(subc => new Subcadenas(m + 1, n, Seq(subc), alfabeto, magnitud, oraculo))
          tasks.foreach(_.fork())
          val results = tasks.map(_.join())
          results.flatten
        } else {
          SC
        }
      }
    }
    val fjPool = new ForkJoinPool()
    val task = new Subcadenas(1, 1, Seq(Seq.empty[Char]), alfabeto, magnitud, oraculo)
    val result = fjPool.invoke(task)
    result.find(_.length == magnitud).getOrElse(Seq())
  }




/*
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




  def reconstruirCadenaTurboAcelerada(magnitud: Int, o: Oraculo): Seq[Char] = {
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

  object Benchmark {

    def compararAlgoritmos(Funcion1: (Seq[Char], Int, Oraculo) => Seq[Char], Funcion2: (Seq[Char], Int, Oraculo) => Seq[Char])(alfabeto:Seq[Char], magnitud:Int, o:Oraculo): (Double, Double, Double) = {
      val timeF1 = withWarmer(new Warmer.Default) measure {
        Funcion1(alfabeto, magnitud, o)
      } //la funcion 1 y 2 reciben dos numeros, los cuales son a y n
      val timeF2 = withWarmer(new Warmer.Default) measure {
        Funcion2(alfabeto, magnitud, o)
      }

      val promedio = timeF1.value / timeF2.value
      (timeF1.value, timeF2.value, promedio)

    } // definimos la funcion para comparar algoritmos con el benchmark


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
      val cadena = reconstruirCadenaIngenuo(alfabeto, magnitud, o)
      val tiempoFinIngenuo = System.nanoTime()
      val tiempoIngenuo = (tiempoFinIngenuo - tiempoInicioIngenuo) / 1e6

      val tiempoInicioIngenuoPar = System.nanoTime()
      val ingenuoPar = reconstruirCadenaIngenuoPar(alfabeto, magnitud, o)
      val tiempoFinIngenuoPar = System.nanoTime()
      val tiempoIngenuoPar = (tiempoFinIngenuoPar - tiempoInicioIngenuoPar) / 1e6

      println(s" Cadena de tamano $magnitud por Ingenuo, secuencial y paralelo: $cadena $ingenuoPar")
      println(s"Tiempo de ejecucion secuencial: $tiempoIngenuo ms, paralelo: $tiempoIngenuoPar ms, aceleracion: ms\n")




      val tiempoInicioMejorado = System.nanoTime()
      val cadenaM = reconstruirCadenaMejorado( alfabeto, magnitud, o)
      val tiempoFinalMejorado = System.nanoTime()
      val tiempoMejorado = (tiempoFinalMejorado - tiempoInicioMejorado) / 1e6
      /*
       val tiempoInicioMejoradoPar = System.nanoTime()
       val Mpar = reconstruirCadenaMejoradoPar(magnitud, o)
       val tiempoFinMejoradoPar = System.nanoTime()
       val tiempoMejoradoPar = (tiempoFinMejoradoPar - tiempoInicioMejoradoPar) / 1e6
       */
       println(s" Cadena de tamano $magnitud por Mejorado, secuencial y paralelo: $cadenaM")
       println(s"Tiempo de ejecucion secuencial: $tiempoFinalMejorado ms, paralelo: ms, aceleracion: ms\n")



      val tiempoInicioTurbo = System.nanoTime()
      val cadenaT = reconstruirCadenaTurbo(alfabeto, magnitud, o)
      val tiempoFinTurbo = System.nanoTime()
      val tiempoTurbo = (tiempoFinTurbo - tiempoInicioTurbo) / 1e6

      val tiempoInicioTurboPar = System.nanoTime()
      val cadenaTP = reconstruirCadenaTurboPar(alfabeto, magnitud, o)
      val tiempoFinTurboPar = System.nanoTime()
      val tiempoTurboPar = (tiempoFinTurboPar - tiempoInicioTurboPar) / 1e6

      println(s" Cadena de tamano $magnitud por Turbo, secuencial y paralelo: $cadenaT, $cadenaTP")
      println(s"Tiempo de ejecucion secuencial: $tiempoTurbo ms, paralelo: $tiempoTurboPar ms, aceleracion ms\n")



      val tiempoInicioTurboMejorado = System.nanoTime()
      val cadenaTM = reconstruirCadenaTurboMejorado(alfabeto, magnitud, o)
      val tiempoFinTurboMejorado = System.nanoTime()
      val tiempoTurboMejorado = (tiempoFinTurboMejorado - tiempoInicioTurboMejorado) / 1e6

      val tiempoInicioTurboMejoradoPar = System.nanoTime()
      val TmejoradoPar = reconstruirCadenaTurboMejoradoPar(alfabeto, magnitud, o)
      val tiempoFinTurboMejoradoPar = System.nanoTime()
      val tiempoTurboMejoradoPar = (tiempoFinTurboMejoradoPar - tiempoInicioTurboMejoradoPar) / 1e6

      println(s" Cadena de tamano $magnitud por Turbo Mejorado, secuencial y paralelo: $cadenaTM, $TmejoradoPar")
      println(s"Tiempo de ejecucion secuencial: $tiempoTurboMejorado ms, paralelo: $tiempoTurboMejoradoPar ms, aceleracion: ms\n")



      val tiempoInicioTurboAcelerado = System.nanoTime()
      val turboAcelerado = reconstruirCadenaTurboAcelerada(magnitud, o)
      val tiempoFinTurboAcelerado = System.nanoTime()
      val tiempoTurboAcelerado = (tiempoFinTurboAcelerado - tiempoInicioTurboAcelerado) / 1e6
      /*
      val tiempoInicioTurboAceleradoPar = System.nanoTime()
      val turboAceleradoPar = recontruirCadenaTurboAceleradaPar(magnitud, o)
      val tiempoFinTurboAceleradoPar = System.nanoTime()
      val tiempoTurboAceleradoPar = (tiempoFinTurboAceleradoPar - tiempoInicioTurboAceleradoPar) / 1e6
      */
      println(s" Cadena de tamano $magnitud por Turbo Acelerado, secuencial y paralelo: $turboAcelerado")
      println(s"Tiempo de ejecucion secuencial: $tiempoTurboAcelerado ms, paralelo: ms, aceleracion: ms\n")


    }
  }
}
