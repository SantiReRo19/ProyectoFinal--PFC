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

    def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
      def generarCadenas(n: Int): Seq[Seq[Char]] = {
        if (n == 0) Seq(Seq())
        else for {
          letra <- alfabeto
          cadena <- generarCadenas(n - 1)
        } yield cadena :+ letra;
      }

      generarCadenas(n).find(o).getOrElse(Seq());
    }

    def secuenciaaleatoria(tamano: Int): String = {
      val random = new Random()
      (1 to tamano).map(_ => alfabeto(random.nextInt(alfabeto.length))).mkString
    }

    def reconstruirCadenaMejorado(alfabeto: Seq[Char], tamano: Int, o: Oraculo): Seq[Char] = {
      def subcaden_candidatas(k: Int, SC: Seq[Seq[Char]]): Seq[Seq[Char]] = {
        if (k > tamano) SC
        else {
          subcaden_candidatas(k + 1, SC.flatMap(subc => alfabeto.map(letra => subc :+ letra)).filter(o))
        }
      }

      val SC = subcaden_candidatas(1, Seq(Seq()))
      SC.find(_.length == tamano).getOrElse(Seq())
    }

    def main(args: Array[String]): Unit = {
      val secuencia = Seq('a', 'c', 'a', 'c')

      val o: Oraculo = (s: Seq[Char]) => {
        secuencia.containsSlice(s)
      }


      val cadenaM = reconstruirCadenaMejorado(alfabeto, 4, o)
      println(s" mejorado Cadena encontrada: $cadenaM")
    }

  }
