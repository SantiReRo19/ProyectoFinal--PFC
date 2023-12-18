package Proyecto
import Proyecto.Algoritmos.{Benchmark, Oraculo, reconstruirCadenaIngenuo, reconstruirCadenaIngenuoPar, reconstruirCadenaMejorado, reconstruirCadenaMejoradoPar, reconstruirCadenaTurbo, reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar, reconstruirCadenaTurboMejorado, reconstruirCadenaTurboMejoradoPar, reconstruirCadenaTurboPar, secuenciaaleatoria}
object CompararX {

  def main(args: Array[String]): Unit = {
    //calentar JVM con withWarmer
    println(
      org.scalameter.withWarmer(new org.scalameter.Warmer.Default) measure {
        (1 to 100000000).toArray
      }
    )

    //Hace la comparacion con un numero presiso de secuencias
    val magnitud = 8
    val SecRandom = secuenciaaleatoria(magnitud)
    val o: Oraculo = (s: Seq[Char]) => {
      SecRandom.containsSlice(s)
    }

    println("COMPARACIONES CON UNA SOLA SECUENCIA:")
    println("Secuencia: " + SecRandom)
    println("\nCOMPARACION INGENUO E INGENIO PAR: ")
    println(Benchmark.compararAlgoritmos(reconstruirCadenaIngenuo, reconstruirCadenaIngenuoPar)(magnitud, o))
    println("\nCOMPARACION MEJORADO Y MEJORADO PAR: ")
    println(Benchmark.compararAlgoritmos(reconstruirCadenaMejorado, reconstruirCadenaMejoradoPar)(magnitud, o))
    println("\nCOMPARACION TURBO Y TURBO PAR: ")
    println(Benchmark.compararAlgoritmos(reconstruirCadenaTurbo, reconstruirCadenaTurboPar)(magnitud, o))
    println("\nCOMPARACION TURBO MEJORADO Y TURBO MEJORADO PAR: ")
    println(Benchmark.compararAlgoritmos(reconstruirCadenaTurboMejorado, reconstruirCadenaTurboMejoradoPar)(magnitud, o))
    println("\nCOMPARACION TURBO ACELERADO Y TURBO ACELERADO PAR: ")
    println(Benchmark.compararAlgoritmos(reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar)(magnitud, o))

  }
}
