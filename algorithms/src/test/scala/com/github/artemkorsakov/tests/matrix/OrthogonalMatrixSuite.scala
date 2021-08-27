package com.github.artemkorsakov.tests.matrix

import com.github.artemkorsakov.matrix.SquaredMatrix
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers._

class OrthogonalMatrixSuite extends AnyFunSuiteLike {
  test("isSymmetrical") {
    SquaredMatrix(Seq(Seq(2, 0), Seq(-1, 3))).isSymmetrical shouldBe false
    SquaredMatrix(Seq(Seq(2, 0), Seq(0, 3))).isSymmetrical shouldBe true
  }

}
