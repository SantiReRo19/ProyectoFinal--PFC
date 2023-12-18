/**
 * Proyecto Final - Programación Funcional y Concurrente
 * Autores: Carlos Alberto Camacho Castaño - 2160331
 *           Juan José Hernandez Arenas - 2259500
 *           Santiago Reyes Rodriguez - 2259738
 * Profesor: Carlos A Delgado
 */
package Proyecto
import Proyecto.pre.{cabezas, raiz}
import common._
import org.scalameter.{Warmer, withWarmer}

import java.util.concurrent.{ForkJoinPool, RecursiveTask}
import scala.annotation.tailrec
import scala.util.Random

object Algoritmos {

//Definimos el alfabeto y el tipo Oraculo
  val alfabeto = Seq('a', 'c', 'g', 't')
  type Oraculo = Seq[Char] => Boolean

//Funcion Auxiliar para generar secuencias aleatorias
  def secuenciaaleatoria(tamano: Int): String = {
    val random = new Random()
    (1 to tamano).map(_ => alfabeto(random.nextInt(alfabeto.length))).mkString
  }

  //Implementaciones secuenciales

  /*
  * Funcion reconstruirCadenaIngenuo
  * @param alfabeto: Seq[Char], longitud: Int, o: Oraculo
  * @return Seq[Char]
  */
  def reconstruirCadenaIngenuo(longitud: Int, o: Oraculo): Seq[Char] = {
    def CadCandidatas(alfabeto: Seq[Char], longitud: Int): Seq[Seq[Char]] = {
      if (longitud == 0) {
        Seq(Seq.empty[Char])
      } else {
        alfabeto.flatMap(caracter => CadCandidatas(alfabeto, longitud - 1).map(caracter +: _))
      }
    }

    val combinacionesPosibles = CadCandidatas(alfabeto, longitud)
    combinacionesPosibles.find(o).getOrElse(Seq())

  }

  /*
  * Funcion reconstruirCadenaMejorado
  * @param alfabeto: Seq[Char], longitud: Int, o: Oraculo
  * @return Seq[Char]
   */
  def reconstruirCadenaMejorado(longitud: Int, o: Oraculo): Seq[Char] = {
    def subcadenas_candidatas(m: Int, SC: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (m <= longitud) subcadenas_candidatas(m + 1, SC.flatMap(subc => alfabeto.map(letra => subc :+ letra)).filter(o))
      else {
        SC
      }
    }
    val SC = subcadenas_candidatas(1, Seq(Seq()))
    SC.find(longitud == _.length).getOrElse(Seq())
  }

  /*
  * Funcion reconstruirCadenaTurbo
  * @param alfabeto: Seq[Char], magnitud: Int, o: Oraculo
  * @return Seq[Char]
   */

  def reconstruirCadenaTurbo(magnitud: Int, o: Oraculo): Seq[Char] = {
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

  /*
  * Funcion reconstruirCadenaTurboMejorado
  * @param alfabeto: Seq[Char], magnitud: Int, o: Oraculo
  * @return Seq[Char]
   */
  def reconstruirCadenaTurboMejorado(magnitud: Int, o: Oraculo): Seq[Char] = {

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

  /*
  * Funcion pertenece
  * @param s: Seq[Char], t: Trie
  * @return Boolean
   */
  /*
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

   */

  def pertenece(sec: Seq[Char], trie: Trie): Boolean = {
    if (sec.isEmpty) {
      trie match {
        case Nodo(_, marcada, _) => marcada
        case Hoja(_, marcada) => marcada
      }
    } else {
      trie match {
        case Nodo(_, _, hijos) => hijos.exists(h => raiz(h) == sec.head && pertenece(sec.tail, h))
        case Hoja(_, _) => false
      }
    }
  }



  /*
  * Funcion adicionar
  * @param s: Seq[Char], trie: Trie
  * @return Trie
   */

  def adicionar(s: Seq[Char], t: Trie): Trie = {
    def nuevaRama(s: Seq[Char]): Trie = {
      s match {
        case cabeza :: cola => cola match {
          case head :: tail => Nodo(cabeza, marcada = false, List(nuevaRama(cola)))
          case Nil => Hoja(cabeza, marcada = true)
        }
        case Nil => Nodo(' ', marcada = false, List())
      }
    }

    def adicionaraux(arbolActual: Trie, charAsignados: Seq[Char], charFaltante: Seq[Char]): Trie = {
      (arbolActual, charAsignados, charFaltante)
      match {
        case (Nodo(car, marcada, hijos), _, head :: tail) => Nodo(car, marcada, hijos :+ nuevaRama(charFaltante))
        case (Nodo(car, false, hijos), _, Nil) => Nodo(car, marcada = true, hijos)
        case (Nodo(car, marcada, hijos), _, head :: tail)
          if cabezas(Nodo(car, marcada, hijos)).contains(head) =>
          val updatedHijos = hijos.map { hijo =>
            if (raiz(hijo) == head){
              adicionaraux(hijo, charAsignados :+ head, tail)
            }
            else hijo
          }
          Nodo(car, marcada, updatedHijos)
        case (Hoja(car, marcada), _, head :: tail) => Nodo(car, marcada,List(nuevaRama(charFaltante)))
        case (_, _, _) => arbolActual
      }
    }
    adicionaraux(t, Seq.empty[Char], s)
  }


  /*
  * Funcion arbolDeSufijos
  * @param sec: Seq[Seq[Char]]
  * @return Trie
   */
  def arbolDeSufijos(sec: Seq[Seq[Char]]): Trie  = {
    sec.foldLeft(Nodo('_', false, List()): Trie)((t, sc) => adicionar(sc, t))
  }

  /*
  * Funcion reconstruirCadenaTurboAcelerada
  * @param tamano: Int, o: Oraculo
  * @return Seq[Char]
   */
  /*
  def reconstruirCadenaTurboAcelerada(alfabeto: Seq[Char], tamano: Int, o: Oraculo): Seq[Char] = {
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
      println(cadenaActual)
      if (cadenaActual.head.length > 2) {
        val Sufijos = arbolDeSufijos(cadenaAnterior)
        cadenaActual.filter { s1 => 0 to s1.length - n forall { i => pertenece(s1.slice(i, i + n), Sufijos) } }
      } else cadenaActual
    }

    val Alfabeto2 = alfabeto.map(Seq(_)).filter(o)
    //transfomar el alfabeto en secuencias de un solo caracter
    println(Alfabeto2)
    val SS = posiblesSucadenas(1, Alfabeto2)
    println(SS)
    SS.head
  }

   */
  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {
    def posiblesSucadenas(j: Int, subCadena: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (j >= n) subCadena
      else {
        val subCadN = subCadena.flatMap { s1 =>
          subCadena.flatMap { s2 =>
            Seq(s1 ++ s2)
          }
        }
        val secActual = filtrar(subCadN, subCadena, j)
        val secFiltrado = secActual.filter(o)
        posiblesSucadenas(j * 2, secFiltrado)
      }
    }

    def filtrar(charActual: Seq[Seq[Char]], charAnt: Seq[Seq[Char]], j: Int): Seq[Seq[Char]] = {
      if (charActual.head.length > 2) {
        val sufijos = arbolDeSufijos(charAnt)
        charActual.filter { sec1 => 0 to sec1.length - j forall { i => pertenece(sec1.slice(i, i + j), sufijos) } }
      } else charActual
    }
    val charAlfabeto = alfabeto.map(Seq(_)).filter(o)
    val subCadenas = posiblesSucadenas(1, charAlfabeto)
    subCadenas.head
  }



  //Implementaciones paralelas


  /*
  * Funcion reconstruirCadenaIngenuoPar
  * @param alfabeto: Seq[Char], longitud: Int, o: Oraculo
  * @return Seq[Char]
   */
  def reconstruirCadenaIngenuoPar(longitud: Int, o: Oraculo): Seq[Char] = {
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
     combinacionesPosibles.find(o).getOrElse(Seq())
  }

  /*
  * Funcion reconstruirCadenaMejoradoPar
  * @param alfabeto: Seq[Char], longitud: Int, o: Oraculo
  * @return Seq[Char]
   */
  def reconstruirCadenaMejoradoPar(longitud: Int, o: Oraculo): Seq[Char] = {
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

  /*
  * Funcion reconstruirCadenaTurboPar
  * @param alfabeto: Seq[Char], magnitud: Int, o: Oraculo
  * @return Seq[Char]
   */
  def reconstruirCadenaTurboPar(magnitud: Int, oraculo: Oraculo): Seq[Char] = {
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
  * Funcion reconstruirCadenaTurboMejoradoPar
  * @param alfabeto: Seq[Char], magnitud: Int, o: Oraculo
  * @return Seq[Char]
   */
  def reconstruirCadenaTurboMejoradoPar(magnitud: Int, oraculo: Oraculo): Seq[Char] = {
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

  /*
  * Funcion reconstruirCadenaTurboAceleradaPar
  * @param tamano: Int, o: Oraculo
  * @return Seq[Char]
   */
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

  /*
  *Objeto Benchmark
  * Funcion compararAlgoritmos
  * @param Funcion1: (Seq[Char], Int, Oraculo) => Seq[Char], Funcion2: (Seq[Char], Int, Oraculo) => Seq[Char]
  * @return (Double, Double, Double)
   */
  object Benchmark {

    def compararAlgoritmos(Funcion1: (Int, Oraculo) => Seq[Char], Funcion2: (Int, Oraculo) => Seq[Char])(magnitud: Int, o: Oraculo): (Double, Double, Double) = {
      val timeF1 = withWarmer(new Warmer.Default) measure {
        Funcion1(magnitud, o)
      }
      val timeF2 = withWarmer(new Warmer.Default) measure {
        Funcion2(magnitud, o)
      }

      val promedio = timeF1.value / timeF2.value
      (timeF1.value, timeF2.value, promedio)

    }

  }

}
