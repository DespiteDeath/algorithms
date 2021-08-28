package com.github.artemkorsakov.matrix

import com.github.artemkorsakov.matrix.GenericOperation._

/** Squared matrix m * n. */
class SquaredMatrix[T](elements: Seq[Seq[T]]) extends Matrix[T](elements) {
  require(isSquared)

  private val zero     = zeroT(topLeft)
  private val one      = oneT(topLeft)
  private val minusOne = minusOneT(topLeft)

  lazy val isSymmetrical: Boolean = this == transpose

  lazy val isSkewSymmetrical: Boolean = this == transpose * minusOne

  lazy val isUpperTriangular: Boolean =
    (0 until n).forall(i => (0 until n).forall(j => j >= i || elements(i)(j) == zero))

  lazy val isLowerTriangular: Boolean =
    (0 until n).forall(i => (0 until n).forall(j => i >= j || elements(i)(j) == zero))

  lazy val isHessenbergMatrix: Boolean =
    (0 until n).forall(i => (0 until n).forall(j => j + 1 >= i || elements(i)(j) == zero))

  lazy val isIdentityMatrix: Boolean =
    (0 until n).forall(i => (0 until n).forall(j => elements(i)(j) == (if (i == j) one else zero)))

  lazy val isOrthogonalMatrix: Boolean =
    (this * this.transpose).toSquaredMatrix.isIdentityMatrix

  /** <a href="https://en.wikipedia.org/wiki/Determinant">Determinant</a> of a matrix. */
  def determinant: T =
    if (elements.length == 1) {
      topLeft
    } else {
      (0 until n).foldLeft(zero) { (sum, i) =>
        val el = elements.head(i)
        val mul =
          if (el == zero) zero
          else mulT(el, SquaredMatrix(elements).minor(0, i).toSquaredMatrix.determinant)
        if (i % 2 == 0) addT(sum, mul) else subT(sum, mul)
      }
    }

  lazy val trace: T = mainDiagonal.foldLeft(zero)(addT)
}

object SquaredMatrix {
  def apply[T](elements: Seq[Seq[T]]): SquaredMatrix[T] = new SquaredMatrix[T](elements)

  def identityMatrix(n: Int): SquaredMatrix[Int] =
    new SquaredMatrix[Int]((0 until n).map(i => (0 until n).map(j => if (i == j) 1 else 0)))
}
