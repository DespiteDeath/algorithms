package com.github.artemkorsakov.matrix

import com.github.artemkorsakov.matrix.GenericOperation.{ minusOneT, oneT, zeroT }

/** Squared matrix m * n. */
class SquaredMatrix[T](elements: Seq[Seq[T]]) extends Matrix[T](elements) {
  require(isSquared)

  lazy val isSymmetrical: Boolean = this == transpose

  lazy val isSkewSymmetrical: Boolean = this == transpose * minusOneT(topLeft)

  lazy val isUpperTriangular: Boolean =
    (0 until n).forall(i => (0 until n).forall(j => j >= i || elements(i)(j) == zeroT(topLeft)))

  lazy val isLowerTriangular: Boolean =
    (0 until n).forall(i => (0 until n).forall(j => i >= j || elements(i)(j) == zeroT(topLeft)))

  lazy val isHessenbergMatrix: Boolean =
    (0 until n).forall(i => (0 until n).forall(j => j + 1 >= i || elements(i)(j) == zeroT(topLeft)))

  lazy val isIdentityMatrix: Boolean =
    (0 until n).forall(i => (0 until n).forall(j => elements(i)(j) == (if (i == j) oneT(topLeft) else zeroT(topLeft))))

  lazy val isOrthogonalMatrix: Boolean =
    (this * this.transpose).toSquaredMatrix.isIdentityMatrix
}

object SquaredMatrix {
  def apply[T](elements: Seq[Seq[T]]): SquaredMatrix[T] = new SquaredMatrix[T](elements)

  def identityMatrix(n: Int): SquaredMatrix[Int] =
    new SquaredMatrix[Int]((0 until n).map(i => (0 until n).map(j => if (i == j) 1 else 0)))
}
