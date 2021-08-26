package com.github.artemkorsakov.tests.matrix

import com.github.artemkorsakov.matrix.Matrix._
import com.github.artemkorsakov.matrix.{ Matrix, MatrixLine }
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers._

class MatrixSuite extends AnyFunSuiteLike {
  test("matrixTranspose") {
    val matrix1 = Seq(Seq(1, 2))
    val matrix2 = Seq(Seq(1), Seq(2))
    matrix1.transpose shouldBe matrix2
    matrix2.transpose shouldBe matrix1
    matrix1.transpose.transpose shouldBe matrix1
    matrix2.transpose.transpose shouldBe matrix2

    val matrix3 = Seq(Seq(1, 2), Seq(3, 4))
    val matrix4 = Seq(Seq(1, 3), Seq(2, 4))
    matrix3.transpose shouldBe matrix4
    matrix4.transpose shouldBe matrix3
    matrix3.transpose.transpose shouldBe matrix3
    matrix4.transpose.transpose shouldBe matrix4

    val matrix5 = Seq(Seq(1, 2), Seq(3, 4), Seq(5, 6))
    val matrix6 = Seq(Seq(1, 3, 5), Seq(2, 4, 6))
    matrix5.transpose shouldBe matrix6
    matrix6.transpose shouldBe matrix5
    matrix5.transpose.transpose shouldBe matrix5
    matrix6.transpose.transpose shouldBe matrix6

    val matrix7 = Seq(Seq(1.1, 2.2), Seq(3.3, 4.4))
    val matrix8 = Seq(Seq(1.1, 3.3), Seq(2.2, 4.4))
    matrix7.transpose shouldBe matrix8
    matrix8.transpose shouldBe matrix7
    matrix7.transpose.transpose shouldBe matrix7
    matrix8.transpose.transpose shouldBe matrix8

    val matrix9  = Seq(Seq(BigInt(1), BigInt(2)), Seq(BigInt(3), BigInt(4)), Seq(BigInt(5), BigInt(6)))
    val matrix10 = Seq(Seq(BigInt(1), BigInt(3), BigInt(5)), Seq(BigInt(2), BigInt(4), BigInt(6)))
    matrix9.transpose shouldBe matrix10
    matrix10.transpose shouldBe matrix9
    matrix9.transpose.transpose shouldBe matrix9
    matrix10.transpose.transpose shouldBe matrix10

    val matrix11 = Seq(Seq(1L, 2L), Seq(3L, 4L), Seq(5L, 6L))
    val matrix12 = Seq(Seq(1L, 3L, 5L), Seq(2L, 4L, 6L))
    matrix11.transpose shouldBe matrix12
    matrix12.transpose shouldBe matrix11
    matrix11.transpose.transpose shouldBe matrix11
    matrix12.transpose.transpose shouldBe matrix12
  }

  test("minorMatrix") {
    val matrix = Seq(Seq(-2, -1, -1, -4), Seq(-1, -2, -1, -6), Seq(-1, -1, 2, 4), Seq(2, 1, -3, -8))
    matrix.minor(0, 0) shouldBe Matrix(Seq(Seq(-2, -1, -6), Seq(-1, 2, 4), Seq(1, -3, -8)))
    matrix.minor(0, 1) shouldBe Matrix(Seq(Seq(-1, -1, -6), Seq(-1, 2, 4), Seq(2, -3, -8)))
    matrix.minor(0, 2) shouldBe Matrix(Seq(Seq(-1, -2, -6), Seq(-1, -1, 4), Seq(2, 1, -8)))
    matrix.minor(0, 3) shouldBe Matrix(Seq(Seq(-1, -2, -1), Seq(-1, -1, 2), Seq(2, 1, -3)))
    matrix.minor(1, 0) shouldBe Matrix(Seq(Seq(-1, -1, -4), Seq(-1, 2, 4), Seq(1, -3, -8)))
    matrix.minor(1, 1) shouldBe Matrix(Seq(Seq(-2, -1, -4), Seq(-1, 2, 4), Seq(2, -3, -8)))
    matrix.minor(1, 2) shouldBe Matrix(Seq(Seq(-2, -1, -4), Seq(-1, -1, 4), Seq(2, 1, -8)))
    matrix.minor(1, 3) shouldBe Matrix(Seq(Seq(-2, -1, -1), Seq(-1, -1, 2), Seq(2, 1, -3)))
    matrix.minor(2, 0) shouldBe Matrix(Seq(Seq(-1, -1, -4), Seq(-2, -1, -6), Seq(1, -3, -8)))
    matrix.minor(2, 1) shouldBe Matrix(Seq(Seq(-2, -1, -4), Seq(-1, -1, -6), Seq(2, -3, -8)))
    matrix.minor(2, 2) shouldBe Matrix(Seq(Seq(-2, -1, -4), Seq(-1, -2, -6), Seq(2, 1, -8)))
    matrix.minor(2, 3) shouldBe Matrix(Seq(Seq(-2, -1, -1), Seq(-1, -2, -1), Seq(2, 1, -3)))
    matrix.minor(3, 0) shouldBe Matrix(Seq(Seq(-1, -1, -4), Seq(-2, -1, -6), Seq(-1, 2, 4)))
    matrix.minor(3, 1) shouldBe Matrix(Seq(Seq(-2, -1, -4), Seq(-1, -1, -6), Seq(-1, 2, 4)))
    matrix.minor(3, 2) shouldBe Matrix(Seq(Seq(-2, -1, -4), Seq(-1, -2, -6), Seq(-1, -1, 4)))
    matrix.minor(3, 3) shouldBe Matrix(Seq(Seq(-2, -1, -1), Seq(-1, -2, -1), Seq(-1, -1, 2)))

    val matrixD = Seq(
      Seq(-2.2, -1.1, -1.1, -4.4),
      Seq(-1.1, -2.2, -1.1, -6.6),
      Seq(-1.1, -1.1, 2.2, 4.4),
      Seq(2.2, 1.1, -3.3, -8.8)
    )
    matrixD.minor(2, 3) shouldBe Matrix(Seq(Seq(-2.2, -1.1, -1.1), Seq(-1.1, -2.2, -1.1), Seq(2.2, 1.1, -3.3)))

    val matrixL = matrix.map(_.map(_.toLong))
    matrixL.minor(2, 3) shouldBe Matrix(Seq(Seq(-2L, -1L, -1L), Seq(-1L, -2L, -1L), Seq(2L, 1L, -3L)))

    val matrixBI = matrix.map(_.map(BigInt(_)))
    matrixBI.minor(2, 3) shouldBe Matrix(
      Seq(
        Seq(BigInt(-2L), BigInt(-1L), BigInt(-1L)),
        Seq(BigInt(-1L), BigInt(-2L), BigInt(-1L)),
        Seq(BigInt(2L), BigInt(1L), BigInt(-3L))
      )
    )
  }

  test("matrixDeterminant") {
    Seq(Seq(-2, -1), Seq(-1, -2)).determinant shouldBe 3
    val matrix = Seq(Seq(-2, -1, -1, -4), Seq(-1, -2, -1, -6), Seq(-1, -1, 2, 4), Seq(2, 1, -3, -8))
    matrix.determinant shouldBe -8
    matrix.map(_.map(_.toLong)).determinant shouldBe -8L
    matrix.map(_.map(_.toDouble)).determinant shouldBe -8.0
    matrix.map(_.map(BigInt(_))).determinant shouldBe BigInt(-8)
  }

  test("add") {
    val matrixA = Seq(Seq(-2, -1, -1, -4), Seq(-1, -2, -1, -6), Seq(-1, -1, 2, 4), Seq(2, 1, -3, -8))
    val matrixB = Seq(Seq(8, -5, -6, -4), Seq(-13, -22, -11, -65), Seq(45, 45, 34, 35), Seq(23, 12, -33, -82))
    val matrixC = Seq(Seq(6, -6, -7, -8), Seq(-14, -24, -12, -71), Seq(44, 44, 36, 39), Seq(25, 13, -36, -90))
    matrixA + matrixB shouldBe Matrix(matrixC)
  }

  test("matrix multiplication by number") {
    Seq(Seq(-2, -1, -1, -4), Seq(-1, -2, -1, -6), Seq(-1, -1, 2, 4), Seq(2, 1, -3, -8)) * 10 shouldBe Matrix(
      Seq(
        Seq(-20, -10, -10, -40),
        Seq(-10, -20, -10, -60),
        Seq(-10, -10, 20, 40),
        Seq(20, 10, -30, -80)
      )
    )
    Seq(Seq(-2, -1, -1, -4), Seq(-1, -2, -1, -6), Seq(-1, -1, 2, 4), Seq(2, 1, -3, -8))
      .*(10, 11) shouldBe Matrix(Seq(Seq(2, 1, 1, 4), Seq(1, 2, 1, 6), Seq(1, 1, 9, 7), Seq(9, 10, 3, 8)))
  }

  test("matrix multiplication") {
    val matrixA = Seq(Seq(3, 4, 2, 5), Seq(0, -1, 3, 2), Seq(1, 2, 3, 0))
    val matrixB = Seq(Seq(1, 2, 3), Seq(-3, 5, 4), Seq(6, 2, 1), Seq(1, -1, 0))
    val matrixC = Seq(Seq(8, 25, 27), Seq(23, -1, -1), Seq(13, 18, 14))
    matrixA * matrixB shouldBe Matrix(matrixC)
    matrixA.*(matrixB, 7) shouldBe Matrix(Seq(Seq(1, 4, 6), Seq(2, 6, 6), Seq(6, 4, 0)))
  }

  test("matrix multiplication by row") {
    val matrixA = Seq(Seq(3, 4, 2, 5), Seq(0, -1, 3, 2), Seq(1, 2, 3, 0))
    val matrixB = Seq(1, -3, 6, 1)
    matrixA * matrixB shouldBe MatrixLine(Seq(8, 23, 13))
    matrixA.*(matrixB, 7) shouldBe MatrixLine(Seq(1, 2, 6))
  }

  test("power") {
    Seq(Seq(2, 0), Seq(-1, 3)).power(2) shouldBe Matrix(Seq(Seq(4, 0), Seq(-5, 9)))

    val fibonacciMatrix = Seq(Seq(1, 1), Seq(1, 0))
    fibonacciMatrix.power(20) shouldBe Matrix(Seq(Seq(10946, 6765), Seq(6765, 4181)))

    Seq(Seq(1, 2, 1, 0), Seq(1, 1, 0, -1), Seq(-2, 0, 1, 2), Seq(0, 2, 1, 1)).power(100) shouldBe Matrix(
      Seq(Seq(1, 200, 100, 0), Seq(100, 1, 0, -100), Seq(-200, 0, 1, 200), Seq(0, 200, 100, 1))
    )

    fibonacciMatrix.power(50, 1000000) shouldBe Matrix(Seq(Seq(11074, 269025), Seq(269025, 742049)))
  }
}
