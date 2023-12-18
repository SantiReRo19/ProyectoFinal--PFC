/**
 * Proyecto Final - Programación Funcional y Concurrente
 * Autores: Carlos Alberto Camacho Castaño - 2160331
 *           Juan José Hernandez Arenas - 2259500
 *           Santiago Reyes Rodriguez - 2259738
 * Profesor: Carlos A Delgado
 */
package Proyecto
import common.parallel
import org.scalameter.{Warmer, withWarmer}
import java.util.concurrent.{ForkJoinPool, RecursiveTask}
import scala.annotation.tailrec
import scala.util.Random

object Proyecto_PFC {

//Definimos el alfabeto y el tipo Oraculo
  val alfabeto = Seq('a', 'c', 'g', 't')
  type Oraculo = Seq[Char] => Boolean

//Funcion Auxiliar para generar secuencias aleatorias
  def secuenciaaleatoria(tamano: Int): String = {
    val random = new Random()
    (1 to tamano).map(_ => alfabeto(random.nextInt(alfabeto.length))).mkString
  }

  //Implementaciones secuenciales
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

  /*
  def pertenecen(s: Seq[Char], t: Trie): Boolean = {
    if (s.isEmpty) {
      t match {
        case Nodo(_, marcada, _) => marcada
        case Hoja(_, marcada) => marcada
      }
    } else {
      t match {
        case Nodo(_, _, hijos) => hijos.exists(h => raiz(h) == s.head && pertenecen(s.tail, h))
        case Hoja(_, _) => false
      }
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

   */

  def pertenece(s: Seq[Char], t: Trie): Boolean = {
    if (s.isEmpty) {
      t match {
        case Nodo(_, marcada, _) => marcada
        case Hoja(_, marcada) => marcada
      }
    } else {
      t match {
        case Nodo(_, _, hijos) => hijos.exists(h => raiz(h) == s.head && pertenece(s.tail, h))
        case Hoja(_, _) => false
      }
    }
  }
  def adicionar(s: Seq[Char], t: Trie): Trie = {
    // Prepara la "rama" a ser agregada al arbol correspondiente a la secuencia o resto de secuencia a ser añadida.
    def crearRama(s: Seq[Char]): Trie = {
      s match {
        case cabeza :: cola => cola match {
          case head :: tail => Nodo(cabeza, marcada = false, List(crearRama(cola)))
          case Nil => Hoja(cabeza, marcada = true)
        }
        case Nil => Nodo(' ', marcada = false, List())
      }
    }

    def agregarRama(arbolActual: Trie, prefix: Seq[Char], remaining: Seq[Char]): Trie = {
      (arbolActual, prefix, remaining) match {
        case (Nodo(car, marcada, hijos), _, head :: tail) if cabezas(Nodo(car, marcada, hijos)).contains(head) =>
          // Recorre recursivamente el árbol hasta llegar al camino deseado
          val updatedHijos = hijos.map { hijo =>
            if (raiz(hijo) == head) agregarRama(hijo, prefix :+ head, tail)
            else hijo
          }
          Nodo(car, marcada, updatedHijos)
        case (Hoja(car, marcada), _, head :: tail) =>
          // Convierte la hoja en un Nodo con el nuevo "subárbol" como hijo
          Nodo(car, marcada, List(crearRama(remaining)))
        case (Nodo(car, marcada, hijos), _, head :: tail) =>
          // Agrega el nuevo nodo a la lista de hijos cuando el camino se detiene en un Nodo
          Nodo(car, marcada, hijos :+ crearRama(remaining))
        case (Nodo(car, false, hijos), _, Nil) =>
          // Modifica el valor de marcada a true si no hay camino por recorrer pero los elementos de la cadena están en el arbol.
          Nodo(car, marcada = true, hijos)
        case (_, _, _) =>
          arbolActual
      }
    }

    agregarRama(t, Seq.empty[Char], s)
  }


  def arbolDeSufijos(secuencias: Seq[Seq[Char]]): Trie  = {
    secuencias.foldLeft(Nodo('_', false, List()): Trie)((t, s) => adicionar(s, t))
  }
  def reconstruirCadenaTurboAcelerada(tamano: Int, o: Oraculo): Seq[Char] = {

    def filtrar(cadenaActual: Seq[Seq[Char]], cadenaAnterior: Seq[Seq[Char]] , k:Int ): Seq[Seq[Char]] = {
      if (cadenaActual.head.length > 2) {
        val t = arbolDeSufijos(cadenaAnterior)
        cadenaActual.filter{s1 => 0 to s1.length - k forall { i => pertenece(s1.slice(i,i+k),t) }}
      } else cadenaActual
    }
    @tailrec
    def subcaden_candidatas(k: Int, SC: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (k >= tamano) SC
      else {
        val SCk = SC.flatMap { s1 =>
          SC.flatMap { s2 =>
            Seq(s1 ++ s2)
          }
        }

        val SCactual = filtrar(SCk, SC, k)
        val SCkFiltrado = SCactual.filter(o)
        subcaden_candidatas(k * 2, SCkFiltrado)
      }
    }
    val Alfab = alfabeto.map(Seq(_)).filter(o)
    val SC = subcaden_candidatas(1, Alfab)
    SC.head
  }




//Implementaciones paralelas

  def reconstruirCadenaIngenuoPar(alfabeto: Seq[Char], longitud: Int, o: Oraculo): Seq[Char] = {
    def CadCandidatas(alfabeto: Seq[Char], longitud: Int): Seq[Seq[Char]] = {
      if (longitud == 0) {
        Seq(Seq.empty[Char])
      } else {
        val n = alfabeto.length / 2
        val (alfabeto1, alfabeto2) = alfabeto.splitAt(n)
        val (p1, p2) = parallel(alfabeto1.flatMap(caracter => CadCandidatas(alfabeto, longitud - 1).map(caracter +: _)),
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

  def reconstruirCadenaMejoradoPar(alfabeto: Seq[Char], longitud: Int, o: Oraculo): Seq[Char] = {
    def subcadenas_candidatasPar(m: Int, SC: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (m <= longitud) {
        val tareas = SC.flatMap(subc => alfabeto.map(letra => task(subc :+ letra)))
        val nuevosCandidatos = tareas.map(_.join()).filter(o)

        subcadenas_candidatasPar(m + 1, nuevosCandidatos)
      } else {
        SC
      }
    }

    val SC = subcadenas_candidatasPar(1, Seq(Seq()))
    SC.find(_.length == longitud).getOrElse(Seq())
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


//Comparacion de algoritmos

  object Benchmark {

    def compararAlgoritmos(Funcion1: (Seq[Char], Int, Oraculo) => Seq[Char], Funcion2: (Seq[Char], Int, Oraculo) => Seq[Char])(alfabeto: Seq[Char], magnitud: Int, o: Oraculo): (Double, Double, Double) = {
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

    //calentar JVM

    val magnitud = 1024
    val SecRandom = secuenciaaleatoria(magnitud)

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

    println(s" Cadena de tamano $magnitud por Ingenuo, secuencial y paralelo: $cadena, $ingenuoPar")
    println(s"Tiempo de ejecucion secuencial: $tiempoIngenuo ms, paralelo: $tiempoIngenuoPar ms.\n")


    val tiempoInicioMejorado = System.nanoTime()
    val cadenaM = reconstruirCadenaMejorado(alfabeto, magnitud, o)
    val tiempoFinalMejorado = System.nanoTime()
    val tiempoMejorado = (tiempoFinalMejorado - tiempoInicioMejorado) / 1e6
    /*
       val tiempoInicioMejoradoPar = System.nanoTime()
       val Mpar = reconstruirCadenaMejoradoPar(magnitud, o)
       val tiempoFinMejoradoPar = System.nanoTime()
       val tiempoMejoradoPar = (tiempoFinMejoradoPar - tiempoInicioMejoradoPar) / 1e6
       */
    println(s" Cadena de tamano $magnitud por Mejorado, secuencial y paralelo: $cadenaM")
    println(s"Tiempo de ejecucion secuencial: $tiempoMejorado ms, paralelo: ms.\n")

    val tiempoInicioMejoradoPar = System.nanoTime()
    val Mpar = reconstruirCadenaMejoradoPar(alfabeto, magnitud, o)
    val tiempoFinMejoradoPar = System.nanoTime()
    val tiempoMejoradoPar = (tiempoFinMejoradoPar - tiempoInicioMejoradoPar) / 1e6

    println(s" Cadena de tamano $magnitud por Mejorado, secuencial y paralelo: $cadenaM, $Mpar")
    println(s"Tiempo de ejecucion secuencial: $tiempoMejorado ms, paralelo: $tiempoMejoradoPar ms.\n")


    val tiempoInicioTurbo = System.nanoTime()
    val cadenaT = reconstruirCadenaTurbo(alfabeto, magnitud, o)
    val tiempoFinTurbo = System.nanoTime()
    val tiempoTurbo = (tiempoFinTurbo - tiempoInicioTurbo) / 1e6

    val tiempoInicioTurboPar = System.nanoTime()
    val cadenaTP = reconstruirCadenaTurboPar(alfabeto, magnitud, o)
    val tiempoFinTurboPar = System.nanoTime()
    val tiempoTurboPar = (tiempoFinTurboPar - tiempoInicioTurboPar) / 1e6

    println(s" Cadena de tamano $magnitud por Turbo, secuencial y paralelo: $cadenaT, $cadenaTP")
    println(s"Tiempo de ejecucion secuencial: $tiempoTurbo ms, paralelo: $tiempoTurboPar ms.\n")


    val tiempoInicioTurboMejorado = System.nanoTime()
    val cadenaTM = reconstruirCadenaTurboMejorado(alfabeto, magnitud, o)
    val tiempoFinTurboMejorado = System.nanoTime()
    val tiempoTurboMejorado = (tiempoFinTurboMejorado - tiempoInicioTurboMejorado) / 1e6

    val tiempoInicioTurboMejoradoPar = System.nanoTime()
    val TmejoradoPar = reconstruirCadenaTurboMejoradoPar(alfabeto, magnitud, o)
    val tiempoFinTurboMejoradoPar = System.nanoTime()
    val tiempoTurboMejoradoPar = (tiempoFinTurboMejoradoPar - tiempoInicioTurboMejoradoPar) / 1e6

    println(s" Cadena de tamano $magnitud por Turbo Mejorado, secuencial y paralelo: $cadenaTM, $TmejoradoPar")
    println(s"Tiempo de ejecucion secuencial: $tiempoTurboMejorado ms, paralelo: $tiempoTurboMejoradoPar ms.\n")


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
    println(s"Tiempo de ejecucion secuencial: $tiempoTurboAcelerado ms, paralelo:  ms.\n")


    /*


    println(s"Secuencia random: $SecRandom")
    println(s"Oraculo: $o")
    println(s"Alfabeto: $alfabeto")

    //Prueba de funcionamiento de la funcion arbolDeSufijos
    val arbol = arbolDeSufijos(Seq(Seq('a', 'c', 't','g'), Seq('a', 'c')))
    println(s"Arbol de sufijos: $arbol")
    println(s"Arbol de sufijos: ${pertenece(Seq('a', 'c', 'g'), arbol)}")
    println(s"Arbol de sufijos: ${pertenece(Seq('a', 'c', 'g', 't'), arbol)}")



     */


  }

}
