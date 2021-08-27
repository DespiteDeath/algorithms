package com.github.artemkorsakov.matrix

class HadamardMatrix(elements: Seq[Seq[Int]]) extends SquaredMatrix[Int](elements) {
  require(elements.forall(row => row.forall(el => el == 1 || el == -1)))

  lazy val isHadamardMatrix: Boolean = {
    val matrix = (this.transpose * this).toSquaredMatrix
    val n      = matrix.n
    val tl     = matrix.topLeft
    val els    = matrix.elements.map(row => row.map(el => el.asInstanceOf[Int]))
    (0 until n).forall(i => (0 until n).forall(j => els(i)(j) == (if (i == j) tl else 0)))
  }

  require(isHadamardMatrix)
}

object HadamardMatrix {
  def apply(elements: Seq[Seq[Int]]): HadamardMatrix = new HadamardMatrix(elements)
}
