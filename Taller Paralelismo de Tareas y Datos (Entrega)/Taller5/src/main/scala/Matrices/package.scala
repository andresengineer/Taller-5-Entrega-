import scala.util.Random
import common._
import scala.collection.parallel.immutable.ParVector

package object Matrices {
  val random = new Random()
  type Matriz = Vector[Vector[Int]]

  def matrizAlAzar(long: Int, vals: Int): Matriz = {
    // Crea una matriz de enteros cuadrada de long x long,
    // con valores aleatorios entre 0 y vals.
    val v = Vector.fill(long, long) { random.nextInt(vals) }
    v
  }

  def vectorAlAzar(long: Int, vals: Int): Vector[Int] = {
    //Crea un vector de enteros de longitud long,
    // con valores aleatorios entre 0 y vals
    val v = Vector.fill(long) { random.nextInt(vals) }
    v
  }

  def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
    (v1 zip v2).map({ case (i, j) => (i * j) }).sum
  }

  def transpuesta(m: Matriz): Matriz = {
    val l = m.length
    Vector.tabulate(l, l)((i, j) => m(j)(i))
  }

  // 1.1.2. Producto cruz (Versión estándar secuencial).
  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val longitud = m1.length
    val m2Transpuesta = transpuesta(m2)
    Vector.tabulate(longitud, longitud)((i, j) => prodPunto(m1(i), m2Transpuesta(j)))
  }


// 1.1.2. Producto cruz (Versión estándar paralela).
  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    // Obtener el número de filas de la matriz m1.
    val filasM1 = m1.length
    // Obtener el número de filas de la matriz m2.
    val filasM2 = m2.length
    // Calcular la matriz transpuesta de la matriz m2.
    val m2Transpuesta = transpuesta(m2)

    // Verificar si el número de filas en m1 es mayor o igual a 4 para aplicar paralelismo.
    if (filasM1 >= 4) {
      // Dividir m1 en dos partes iguales, m1Derecha y m1Izquierda.
      val mitad = filasM1 / 2
      val (m1Derecha, m1Izquierda) = m1.splitAt(mitad)

      // Crear tareas paralelas para multiplicar m1Derecha y m1Izquierda por m2.
      val resultado1 = task {
        multMatrizPar(m1Derecha, m2)
      }
      val resultado2 = task {
        multMatrizPar(m1Izquierda, m2)
      }

      // Obtener los resultados de las tareas paralelas.
      val resultadoDerecha = resultado1.join()
      val resultadoIzquierda = resultado2.join()

      // Concatenar los resultados de las matrices multiplicadas.
      resultadoDerecha ++ resultadoIzquierda
    } else {
      // Calcular la matriz resultante en serie utilizando producto punto entre filas de m1 y columnas de m2 transpuesta.
      Vector.tabulate(filasM1, filasM2) { (i, j) =>
        prodPunto(m1(i), m2Transpuesta(j))
      }
    }
  }

  // 1.2.1. Extrayendo submatrices.
  def subMatriz(m:Matriz, i:Int, j:Int, l:Int):Matriz = {
    // Dada m, matriz cuadrada de NxN, 1<=i , j<=N, i+n<=N, j+n<=N,
    // devuelve la submatriz de nxn correspondiente a m[i..i+(n-1), j..j+(n-1)]
    Vector.tabulate(l, l)((x, y) => m(i+x)(y+j))
  }

  // 1.2.2. Sumando matrices.
  def sumMatriz(m1:Matriz, m2:Matriz):Matriz = {
    // recibe m1 y m2 matrices cuadradas de la misma dimension, potencia de 2
    // y de vuelve la matriz resultante de la suma de las 2 matrices
    val l = m1.length
    Vector.tabulate(l, l)((i, j) => m1(i)(j) + m2(i)(j))
  }

  // 1.2.3 Multiplicando matrices recursivamente (Versión secuencial).
  def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {
    // recibe m1 y m2 matrices cuadradas de la misma dimension, potencia de 2
    // y devuelve la multiplicacion de las 2 matrices
    val l = m1.length
    val mitad = l/2

    if (l == 1) {
      // Caso base: multiplicación de matrices de 1x1
      Vector.tabulate(l, l)((i, j) => prodPunto(m1(i), m2(j)))
    } else {
      // Realizo subdivisiones una por una
      val A11 = subMatriz(m1, 0, 0, mitad)
      val A12 = subMatriz(m1, 0, mitad, mitad)
      val A21 = subMatriz(m1, mitad, 0, mitad)
      val A22 = subMatriz(m1, mitad, mitad, mitad)

      val B11 = subMatriz(m2, 0, 0, mitad)
      val B12 = subMatriz(m2, 0, mitad, mitad)
      val B21 = subMatriz(m2, mitad, 0, mitad)
      val B22 = subMatriz(m2, mitad, mitad, mitad)

      // Multiplico las subdivisiones y guardo resultados
      val C11 = sumMatriz(multMatrizRec(A11, B11), multMatrizRec(A12, B21))
      val C12 = sumMatriz(multMatrizRec(A11, B12), multMatrizRec(A12, B22))
      val C21 = sumMatriz(multMatrizRec(A21, B11), multMatrizRec(A22, B21))
      val C22 = sumMatriz(multMatrizRec(A21, B12), multMatrizRec(A22, B22))

      // Uno los resultados en dos partes
      val part1 = C11.zip(C12).map { case (row1, row2) => row1 ++ row2 }
      val part2 = C21.zip(C22).map { case (row1, row2) => row1 ++ row2 }

      // Uno las dos mitades para obtener el resultado final
      part1 ++ part2
    }
  }

  // 1.2.4 Multiplicando matrices recursivamente (Versión paralela).
  def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
    // recibe m1 y m2 matrices cuadradas de la misma dimension, potencia de 2
    // y devuelve la multiplicacion de las 2 matrices, paralelizando tareas.
    val l = m1.length
    val mitad = l/2
    val umbral = 128

    // Caso base: multiplicación de matrices de 1x1
    if (l == 1) {
      return Vector.tabulate(l, l)((i, j) => prodPunto(m1(i), m2(j)))
    }

    // Establezco un umbral
    if (m1.length <= umbral) {
      return multMatrizRec(m1, m2)
    }

    // Realizo subdivisiones una por una
    val A11 = subMatriz(m1, 0, 0, mitad)
    val A12 = subMatriz(m1, 0, mitad, mitad)
    val A21 = subMatriz(m1, mitad, 0, mitad)
    val A22 = subMatriz(m1, mitad, mitad, mitad)

    val B11 = subMatriz(m2, 0, 0, mitad)
    val B12 = subMatriz(m2, 0, mitad, mitad)
    val B21 = subMatriz(m2, mitad, 0, mitad)
    val B22 = subMatriz(m2, mitad, mitad, mitad)

    // Multiplico las subdivisiones y guardo resultados en tareas (task)
    val tC11 = task(sumMatriz(multMatrizRecPar(A11, B11), multMatrizRecPar(A12, B21)))
    val tC12 = task(sumMatriz(multMatrizRecPar(A11, B12), multMatrizRecPar(A12, B22)))
    val tC21 = task(sumMatriz(multMatrizRecPar(A21, B11), multMatrizRecPar(A22, B21)))
    val tC22 = task(sumMatriz(multMatrizRecPar(A21, B12), multMatrizRecPar(A22, B22)))

    // Hago join a los tasks
    val C11 = tC11.join()
    val C12 = tC12.join()
    val C21 = tC21.join()
    val C22 = tC22.join()

    // Uno los resultados en dos partes
    val part1 = C11.zip(C12).map { case (row1, row2) => row1 ++ row2 }
    val part2 = C21.zip(C22).map { case (row1, row2) => row1 ++ row2 }

    // Uno las dos mitades para obtener el resultado final
    part1 ++ part2
  }

  // 1.3.1. Restando matrices.
  def restaMatriz(m1: Matriz, m2: Matriz): Matriz = {
    // recibe m1 y m2 matrices cuadradas de la misma dimension, potencia de 2
    // y devuelve la matriz resultante de la resta de las 2 matrices
    val l = m1.length
    Vector.tabulate(l, l)((i, j) => m1(i)(j) - m2(i)(j))
  }

  // 1.3.2. Algoritmo de Strassen (versión secuencial)
  def multStrassen(m1: Matriz, m2: Matriz): Matriz = {
    // recibe m1 y m2 matrices cuadradas de la misma dimension, potencia de 2
    // y devuelve la multiplicacion de las 2 matrices usando el algoritmo de Strassen
    val l = m1.length
    val mitad = l / 2

    // Caso base: Matrices de 1x1
    if (l == 1) {
      // Multiplicación simple
      return Vector.tabulate(l, l)((i, j) => prodPunto(m1(i), m2(j)))
    } else {

      // Subdivisión de las matrices de entrada
      val A11 = subMatriz(m1, 0, 0, mitad)
      val A12 = subMatriz(m1, 0, mitad, mitad)
      val A21 = subMatriz(m1, mitad, 0, mitad)
      val A22 = subMatriz(m1, mitad, mitad, mitad)

      val B11 = subMatriz(m2, 0, 0, mitad)
      val B12 = subMatriz(m2, 0, mitad, mitad)
      val B21 = subMatriz(m2, mitad, 0, mitad)
      val B22 = subMatriz(m2, mitad, mitad, mitad)

      // Cálculos de las matrices S y P
      val S1 = restaMatriz(B12, B22)
      val S2 = sumMatriz(A11, A12)
      val S3 = sumMatriz(A21, A22)
      val S4 = restaMatriz(B21, B11)
      val S5 = sumMatriz(A11, A22)
      val S6 = sumMatriz(B11, B22)
      val S7 = restaMatriz(A12, A22)
      val S8 = sumMatriz(B21, B22)
      val S9 = restaMatriz(A11, A21)
      val S10 = sumMatriz(B11, B12)

      val P1 = multStrassen(A11, S1)
      val P2 = multStrassen(S2, B22)
      val P3 = multStrassen(S3, B11)
      val P4 = multStrassen(A22, S4)
      val P5 = multStrassen(S5, S6)
      val P6 = multStrassen(S7, S8)
      val P7 = multStrassen(S9, S10)

      // Cálculos de las matrices C
      val C11 = restaMatriz(sumMatriz(sumMatriz(P5, P4), P6), P2)
      val C12 = sumMatriz(P1, P2)
      val C21 = sumMatriz(P3, P4)
      val C22 = restaMatriz(restaMatriz(sumMatriz(P5, P1), P3), P7)

      // Combinación de resultados en dos partes
      val parte1 = C11.zip(C12).map { case (fila1, fila2) => fila1 ++ fila2 }
      val parte2 = C21.zip(C22).map { case (fila1, fila2) => fila1 ++ fila2 }

      // Uno las dos mitades para obtener el resultado final
      parte1 ++ parte2
    }
  }

  // 1.3.2. Algoritmo de Strassen (versión paralela)
  def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {
    // recibe m1 y m2 matrices cuadradas de la misma dimension, potencia de 2
    // y devuelve la multiplicacion de las 2 matrices usando el algoritmo de Strassen en paralelo
    val l = m1.length
    val mitad = l / 2
    val umbral = 128

    // Caso base: Matrices de 1x1
    if (l == 1) {
      // Multiplicación simple
      return Vector.tabulate(l, l)((i, j) => prodPunto(m1(i), m2(j)))
    } else {
      // Establezco un umbral
      if (l <= umbral) {
        return multStrassen(m1, m2)
      } else {
        // Subdivisión de las matrices de entrada
        val A11 = subMatriz(m1, 0, 0, mitad)
        val A12 = subMatriz(m1, 0, mitad, mitad)
        val A21 = subMatriz(m1, mitad, 0, mitad)
        val A22 = subMatriz(m1, mitad, mitad, mitad)

        val B11 = subMatriz(m2, 0, 0, mitad)
        val B12 = subMatriz(m2, 0, mitad, mitad)
        val B21 = subMatriz(m2, mitad, 0, mitad)
        val B22 = subMatriz(m2, mitad, mitad, mitad)

        // Cálculos de las matrices S
        val S1 = restaMatriz(B12, B22)
        val S2 = sumMatriz(A11, A12)
        val S3 = sumMatriz(A21, A22)
        val S4 = restaMatriz(B21, B11)
        val S5 = sumMatriz(A11, A22)
        val S6 = sumMatriz(B11, B22)
        val S7 = restaMatriz(A12, A22)
        val S8 = sumMatriz(B21, B22)
        val S9 = restaMatriz(A11, A21)
        val S10 = sumMatriz(B11, B12)

        // Cálculos de las matrices P (en paralelo usando task)
        val tP1 = task(multStrassen(A11, S1))
        val tP2 = task(multStrassen(S2, B22))
        val tP3 = task(multStrassen(S3, B11))
        val tP4 = task(multStrassen(A22, S4))
        val tP5 = task(multStrassen(S5, S6))
        val tP6 = task(multStrassen(S7, S8))
        val tP7 = task(multStrassen(S9, S10))

        // Realizo el join de las matrices P
        val P1 = tP1.join()
        val P2 = tP2.join()
        val P3 = tP3.join()
        val P4 = tP4.join()
        val P5 = tP5.join()
        val P6 = tP6.join()
        val P7 = tP7.join()

        // Cálculos de las matrices C
        val C11 = restaMatriz(sumMatriz(sumMatriz(P5, P4), P6), P2)
        val C12 = sumMatriz(P1, P2)
        val C21 = sumMatriz(P3, P4)
        val C22 = restaMatriz(restaMatriz(sumMatriz(P5, P1), P3), P7)

        // Combinación de resultados en dos partes
        val part1 = Vector.tabulate(C11.length)(i => C11(i) ++ C12(i))
        val part2 = Vector.tabulate(C21.length)(i => C21(i) ++ C22(i))

        // Uno las dos mitades para obtener el resultado final.
        part1 ++ part2
      }
    }
  }

  // 1.5. Implementando el producto punto usando paralelismo de datos.
  def prodPuntoParD(v1: ParVector[Int], v2: ParVector[Int]): Int = {
    (v1 zip v2).map({ case (i, j) => (i * j) }).sum
  }


}
