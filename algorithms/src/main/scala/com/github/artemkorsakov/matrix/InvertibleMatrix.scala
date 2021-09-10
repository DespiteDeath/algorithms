package com.github.artemkorsakov.matrix

import com.github.artemkorsakov.numbers.RationalNumber

class InvertibleMatrix(elements: Seq[Seq[RationalNumber]]) extends SquaredMatrix[RationalNumber](elements) {
  val det: RationalNumber = determinant
  require(det != RationalNumber(0))

  lazy val invertibleMatrix: InvertibleMatrix =
    InvertibleMatrix(
      (0 until n).map(i =>
        (0 until n).map { j =>
          val sign = if ((i + j) % 2 == 0) 1 else -1
          RationalNumber(sign) * minor(j, i).toSquaredMatrix.determinant / det
        }
      )
    )
}

object InvertibleMatrix {
  def intSeqToInvertibleMatrix(elements: Seq[Seq[Int]]): InvertibleMatrix =
    InvertibleMatrix(elements.map(_.map(RationalNumber(_))))

  def longSeqToInvertibleMatrix(elements: Seq[Seq[Long]]): InvertibleMatrix =
    InvertibleMatrix(elements.map(_.map(RationalNumber(_))))

  def apply(elements: Seq[Seq[RationalNumber]]): InvertibleMatrix = new InvertibleMatrix(elements)

}
