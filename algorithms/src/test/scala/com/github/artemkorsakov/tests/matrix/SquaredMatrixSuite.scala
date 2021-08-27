package com.github.artemkorsakov.tests.matrix

import com.github.artemkorsakov.matrix.SquaredMatrix
import com.github.artemkorsakov.numbers.RationalNumber
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers._

class SquaredMatrixSuite extends AnyFunSuiteLike {
  test("isSymmetrical") {
    SquaredMatrix(Seq(Seq(2, 0), Seq(-1, 3))).isSymmetrical shouldBe false
    SquaredMatrix(Seq(Seq(2, 0), Seq(0, 3))).isSymmetrical shouldBe true
  }

  test("isSkewSymmetrical") {
    SquaredMatrix(Seq(Seq(2, 0), Seq(-1, 3))).isSkewSymmetrical shouldBe false
    SquaredMatrix(Seq(Seq(2, 0), Seq(0, 3))).isSkewSymmetrical shouldBe false
    SquaredMatrix(Seq(Seq(0, 1), Seq(-1, 0))).isSkewSymmetrical shouldBe true
  }

  test("isUpperTriangular") {
    SquaredMatrix(Seq(Seq(2, 0), Seq(-1, 3))).isUpperTriangular shouldBe false
    SquaredMatrix(Seq(Seq(2, 10), Seq(0, 3))).isUpperTriangular shouldBe true
  }

  test("isLowerTriangular") {
    SquaredMatrix(Seq(Seq(2, 10), Seq(-1, 3))).isLowerTriangular shouldBe false
    SquaredMatrix(Seq(Seq(2, 0), Seq(0, 3))).isLowerTriangular shouldBe true
  }

  test("isHessenbergMatrix") {
    SquaredMatrix(Seq(Seq(2, 10, 0), Seq(0, 3, 1), Seq(1, 0, 3))).isHessenbergMatrix shouldBe false
    SquaredMatrix(Seq(Seq(2, 10, 0), Seq(1, 3, 1), Seq(0, 1, 3))).isHessenbergMatrix shouldBe true
  }

  test("isIdentityMatrix") {
    SquaredMatrix(Seq(Seq(1, 0, 0), Seq(0, 1, 0), Seq(1, 0, 1))).isIdentityMatrix shouldBe false
    SquaredMatrix(Seq(Seq(1, 0, 0), Seq(0, 1, 0), Seq(0, 0, 1))).isIdentityMatrix shouldBe true
  }

  test("isOrthogonalMatrix") {
    SquaredMatrix(Seq(Seq(2, 0, 0), Seq(0, 1, 0), Seq(0, 0, 1))).isOrthogonalMatrix shouldBe false
    SquaredMatrix(Seq(Seq(1, 0, 0), Seq(0, 1, 0), Seq(0, 0, 1))).isOrthogonalMatrix shouldBe true
    SquaredMatrix(
      Seq(
        Seq(RationalNumber(1, 3), RationalNumber(1, 2), RationalNumber(1, 2), RationalNumber(1, 2)),
        Seq(RationalNumber(1, 2), RationalNumber(-1, 2), RationalNumber(1, 2), RationalNumber(-1, 2)),
        Seq(RationalNumber(1, 2), RationalNumber(1, 2), RationalNumber(-1, 2), RationalNumber(-1, 2)),
        Seq(RationalNumber(1, 2), RationalNumber(-1, 2), RationalNumber(-1, 2), RationalNumber(1, 2))
      )
    ).isOrthogonalMatrix shouldBe false
    SquaredMatrix(
      Seq(
        Seq(RationalNumber(1, 2), RationalNumber(1, 2), RationalNumber(1, 2), RationalNumber(1, 2)),
        Seq(RationalNumber(1, 2), RationalNumber(-1, 2), RationalNumber(1, 2), RationalNumber(-1, 2)),
        Seq(RationalNumber(1, 2), RationalNumber(1, 2), RationalNumber(-1, 2), RationalNumber(-1, 2)),
        Seq(RationalNumber(1, 2), RationalNumber(-1, 2), RationalNumber(-1, 2), RationalNumber(1, 2))
      )
    ).isOrthogonalMatrix shouldBe true
  }

  test("matrixDeterminant") {
    SquaredMatrix(Seq(Seq(-2, -1), Seq(-1, -2))).determinant shouldBe 3
    val matrix = SquaredMatrix(Seq(Seq(-2, -1, -1, -4), Seq(-1, -2, -1, -6), Seq(-1, -1, 2, 4), Seq(2, 1, -3, -8)))
    matrix.determinant shouldBe -8
    matrix.trace shouldBe -10
  }

  test("identityMatrix") {
    SquaredMatrix.identityMatrix(2) shouldBe SquaredMatrix(Seq(Seq(1, 0), Seq(0, 1)))
    SquaredMatrix.identityMatrix(3) shouldBe SquaredMatrix(Seq(Seq(1, 0, 0), Seq(0, 1, 0), Seq(0, 0, 1)))
    SquaredMatrix.identityMatrix(2).isIdentityMatrix shouldBe true
  }
}
