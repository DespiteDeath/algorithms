package com.github.artemkorsakov.numbers

import com.github.artemkorsakov.matrix.Matrix

/** <a href="https://en.wikipedia.org/wiki/Fibonacci_number">Fibonacci number</a> */
object Fibonacci {
  private val PHI: BigDecimal                  = BigDecimal((1.0 + math.sqrt(5.0)) / 2.0)
  private val fibonacci_matrix: Matrix[BigInt] = Matrix(Seq(Seq(1, 1), Seq(1, 0)))

  /** Returns approximate F<sub>n</sub> - n-th fibonacci number */
  def approximateFibonacci(n: Int): BigInt = approximateFibonacci(n.toLong)

  def approximateFibonacci(n: Long): BigInt =
    (PHI.pow(n.toInt) / BigDecimal(math.sqrt(5))).setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt

  def fibonacci(n: Int): BigInt = fibonacci(n.toLong)

  def fibonacci(n: Long): BigInt = fibonacciMatrix(n).row(0).last

  def fibonacciMatrix(n: Long): Matrix[BigInt] = fibonacci_matrix.power(n)

  def fibonacciMatrix(n: Long, module: BigInt): Matrix[BigInt] = fibonacci_matrix.power(n, module)
}
