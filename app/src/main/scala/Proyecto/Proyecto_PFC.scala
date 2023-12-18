/**
 * Proyecto Final - Programación Funcional y Concurrente
 * Autores: Carlos Alberto Camacho Castaño - 2160331
 *           Juan José Hernandez Arenas - 2259500
 *           Santiago Reyes Rodriguez - 2259738
 * Profesor: Carlos A Delgado
 */
package Proyecto
import common._
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

  def pertenece(s: Seq[Char], t: Trie): Boolean = {
    if (s.isEmpty) {
      t match {
        case Nodo(_, marca, _) => marca
        case Hoja(_, marca) => marca
      }
    } else {
      t match {
        case Nodo(_, _, hijos) => hijos.exists(h => pre.raiz(h) == s.head && pertenece(s.tail, h))
        case Hoja(_, _) => false
      }
    }
  }
  def adicionar(s: Seq[Char], trie: Trie): Trie = {
    def crearRama(s: Seq[Char]): Trie = {
      s match {
        case head :: tail => tail match {
          case head :: tail => Nodo(head, marcada = false, List(crearRama(tail)))
          case Nil => Hoja(head, marcada = true)
        }
        case Nil => Nodo(' ', marcada = false, List())
      }
    }

    def añadirRama(arbolActual: Trie, charAsignados: Seq[Char], charRestantes: Seq[Char]): Trie = {
      (arbolActual, charAsignados, charRestantes) match {
        case (Nodo(char, marca, hijos), _, head :: tail) if pre.cabezas(Nodo(char, marca, hijos)).contains(head) =>
          val nuevoTrie = hijos.map { hijo =>
            if (pre.raiz(hijo) == head) añadirRama(hijo, charAsignados :+ head, tail)
            else hijo
          }
          Nodo(char, marca, nuevoTrie)
        case (Hoja(char, marcada), _, head :: tail) =>
          Nodo(char, marcada, List(crearRama(charRestantes)))
        case (Nodo(char, marcada, hijos), _, head :: tail) =>
          Nodo(char, marcada, hijos :+ crearRama(charRestantes))
        case (Nodo(char, false, hijos), _, Nil) =>
          Nodo(char, marcada = true, hijos)
        case (_, _, _) =>
          arbolActual
      }
    }
    añadirRama(trie, Seq.empty[Char], s)
  }

  def arbolDeSufijos(sec: Seq[Seq[Char]]): Trie  = {
    sec.foldLeft(Nodo('_', false, List()): Trie)((trie, sc) => adicionar(sc, trie))
  }
  def reconstruirCadenaTurboAcelerada(tamano: Int, o: Oraculo): Seq[Char] = {
    @tailrec
    def posiblesSucadenas(n: Int, sec: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (n >= tamano) sec
      else {
        val secN = sec.flatMap { s1 =>
          sec.flatMap { s2 =>
          Seq(s1 ++ s2)
          }
        }
        val secActual = filtrar(secN, sec, n)
        val secNFiltrado = secActual.filter(o)
        posiblesSucadenas(n * 2, secNFiltrado)
      }
    }

    def filtrar(cadenaActual: Seq[Seq[Char]], cadenaAnterior: Seq[Seq[Char]], n: Int): Seq[Seq[Char]] = {
      if (cadenaActual.head.length > 2) {
        val Sufijos = arbolDeSufijos(cadenaAnterior)
        cadenaActual.filter { s1 => 0 to s1.length - n forall { i => pertenece(s1.slice(i, i + n), Sufijos) } }
      } else cadenaActual
    }

    val Alfabeto2 = alfabeto.map(Seq(_)).filter(o)
    val SS = posiblesSucadenas(1, Alfabeto2)
    SS.head
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

  def reconstruirCadenaTurboAceleradaPar(tamano: Int, o: Oraculo): Seq[Char] = {

    @tailrec
    def posiblesSucadenas(n: Int, sec: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (n >= tamano) sec
      else {
        val (sc1,sc2) = sec.splitAt(sec.length/2)
        val (p1,p2) = parallel(sc1.flatMap { s1 =>
          sec.flatMap { s2 =>
            Seq(s1 ++ s2)
          }
        }, sc2.flatMap { s1 =>
          sec.flatMap { s2 =>
            Seq(s1 ++ s2)
          }
        })
        val secN = p1 ++ p2

        val secActual = filtrar(secN, sec, n)
        val secNFiltrado = secActual.filter(o)
        posiblesSucadenas(n * 2, secNFiltrado)
      }
    }

    def filtrar(cadActual: Seq[Seq[Char]], cadPasada: Seq[Seq[Char]], n:Int ): Seq[Seq[Char]] = {
      if (cadActual.head.length > 2) {
        val Sufijos = arbolDeSufijos(cadPasada)
        val (cad1,cad2) = cadActual.splitAt(cadActual.length/2)
        val (p1,p2) = parallel(cad1.filter{s1 => 0 to s1.length - n forall { i => pertenece(s1.slice(i,i+n),Sufijos) }},
          cad2.filter{s1 => 0 to s1.length - n forall { i => pertenece(s1.slice(i,i+n),Sufijos) }})
        p1 ++ p2
      } else cadActual
    }

    val Alfabeto2 = alfabeto.map(Seq(_)).filter(o)
    val SS = posiblesSucadenas(1, Alfabeto2)
    SS.head

  }

  //Comparacion
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

}
