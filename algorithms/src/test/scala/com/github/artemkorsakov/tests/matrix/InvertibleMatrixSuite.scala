package com.github.artemkorsakov.tests.matrix

import com.github.artemkorsakov.matrix.InvertibleMatrix.intSeqToInvertibleMatrix
import com.github.artemkorsakov.numbers.RationalNumber
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class InvertibleMatrixSuite extends AnyFunSuiteLike {
  test("invertibleMatrix") {
    val matrix    = intSeqToInvertibleMatrix(Seq(Seq(3, 2, 4), Seq(4, 3, 6), Seq(6, 4, 9)))
    val invMatrix = matrix.invertibleMatrix
    val iMatrix   = matrix * invMatrix
    invMatrix.shouldBe(intSeqToInvertibleMatrix(Seq(Seq(3, -2, 0), Seq(0, 3, -2), Seq(-2, 0, 1))))
    iMatrix.shouldBe(intSeqToInvertibleMatrix(Seq(Seq(1, 0, 0), Seq(0, 1, 0), Seq(0, 0, 1))))
  }

  test("solveEquation") {
    val matrix   = intSeqToInvertibleMatrix(Seq(Seq(1, 5), Seq(2, 6)))
    val solution = matrix.solveEquation(Seq(7, 10).map(RationalNumber(_)))
    solution.shouldBe(Seq(RationalNumber(2), RationalNumber(1)))
  }
}
