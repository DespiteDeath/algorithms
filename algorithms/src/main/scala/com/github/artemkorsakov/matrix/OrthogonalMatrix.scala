package com.github.artemkorsakov.matrix

/** Orthogonal matrix m * n. */
class OrthogonalMatrix[T](elements: Seq[Seq[T]]) extends SquaredMatrix[T](elements) {
  require(isOrthogonalMatrix)

}

object OrthogonalMatrix {
  def apply[T](elements: Seq[Seq[T]]): OrthogonalMatrix[T] = new OrthogonalMatrix[T](elements)
}
