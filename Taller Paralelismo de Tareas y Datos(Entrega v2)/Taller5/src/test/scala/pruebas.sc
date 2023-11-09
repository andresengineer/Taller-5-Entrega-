/*
Taller #5:

Wilson Andrés Mosquera Zapata <202182116>
Angie Natalia Córdoba Collazos <202124366>

9/11/2023

Archivo: pruebas.sc

*/

import Benchmark._
import Matrices._

// CASOS DE PRUEBA:

// Matrices 2x2
val m1 = matrizAlAzar(2, 2)
val m2 = matrizAlAzar(2, 2)

multMatriz(m1,m2)
multMatrizPar(m1,m2)
multMatrizRec(m1,m2)
multMatrizRecPar(m1,m2)
multStrassen(m1,m2)
multStrassenPar(m1,m2)

//Comparaciones versiones secuencial y paralelo
val Estandar = compararAlgoritmos(multMatriz,multMatrizPar)(m1,m2)
val Recursiva = compararAlgoritmos(multMatrizRec,multMatrizRecPar)(m1,m2)
val Strassen = compararAlgoritmos(multStrassen,multStrassenPar)(m1,m2)

// Matrices 8x8
val m1 = matrizAlAzar(8, 2)
val m2 = matrizAlAzar(8, 2)

multMatriz(m1,m2)
multMatrizPar(m1,m2)
multMatrizRec(m1,m2)
multMatrizRecPar(m1,m2)
multStrassen(m1,m2)
multStrassenPar(m1,m2)

//Comparaciones versiones secuencial y paralelo
val Estandar = compararAlgoritmos(multMatriz,multMatrizPar)(m1,m2)
val Recursiva = compararAlgoritmos(multMatrizRec,multMatrizRecPar)(m1,m2)
val Strassen = compararAlgoritmos(multStrassen,multStrassenPar)(m1,m2)

// Matrices 64x64
val m1 = matrizAlAzar(64, 2)
val m2 = matrizAlAzar(64, 2)

multMatriz(m1,m2)
multMatrizPar(m1,m2)
multMatrizRec(m1,m2)
multMatrizRecPar(m1,m2)
multStrassen(m1,m2)
multStrassenPar(m1,m2)

//Comparaciones versiones secuencial y paralelo
val Estandar = compararAlgoritmos(multMatriz,multMatrizPar)(m1,m2)
val Recursiva = compararAlgoritmos(multMatrizRec,multMatrizRecPar)(m1,m2)
val Strassen = compararAlgoritmos(multStrassen,multStrassenPar)(m1,m2)

// Matrices 128x128
val m1 = matrizAlAzar(128, 2)
val m2 = matrizAlAzar(128, 2)

multMatriz(m1,m2)
multMatrizPar(m1,m2)
multMatrizRec(m1,m2)
multMatrizRecPar(m1,m2)
multStrassen(m1,m2)
multStrassenPar(m1,m2)

//Comparaciones versiones secuencial y paralelo
val Estandar = compararAlgoritmos(multMatriz,multMatrizPar)(m1,m2)
val Recursiva = compararAlgoritmos(multMatrizRec,multMatrizRecPar)(m1,m2)
val Strassen = compararAlgoritmos(multStrassen,multStrassenPar)(m1,m2)

// Matrices 256x256
val m1 = matrizAlAzar(256, 2)
val m2 = matrizAlAzar(256, 2)

multMatriz(m1,m2)
multMatrizPar(m1,m2)
multMatrizRec(m1,m2)
multMatrizRecPar(m1,m2)
multStrassen(m1,m2)
multStrassenPar(m1,m2)

//Comparaciones versiones secuencial y paralelo
val Estandar = compararAlgoritmos(multMatriz,multMatrizPar)(m1,m2)
val Recursiva = compararAlgoritmos(multMatrizRec,multMatrizRecPar)(m1,m2)
val Strassen = compararAlgoritmos(multStrassen,multStrassenPar)(m1,m2)

// Matrices 512x512
val m1 = matrizAlAzar(512, 2)
val m2 = matrizAlAzar(512, 2)

multMatriz(m1,m2)
multMatrizPar(m1,m2)
multMatrizRec(m1,m2)
multMatrizRecPar(m1,m2)
multStrassen(m1,m2)
multStrassenPar(m1,m2)

//Comparaciones versiones secuencial y paralelo
val Estandar = compararAlgoritmos(multMatriz,multMatrizPar)(m1,m2)
val Recursiva = compararAlgoritmos(multMatrizRec,multMatrizRecPar)(m1,m2)
val Strassen = compararAlgoritmos(multStrassen,multStrassenPar)(m1,m2)

// 1.5. Implementando el producto punto usando paralelismo de datos. (pruebas)
/*
compararProdPunto(2)
compararProdPunto(16)
compararProdPunto(32)
compararProdPunto(128)
compararProdPunto(256)
compararProdPunto(1024)
compararProdPunto(2048)
compararProdPunto(4096)
compararProdPunto(8192)
compararProdPunto(16384)
compararProdPunto(32768)
compararProdPunto(65536)
compararProdPunto(131072)*/