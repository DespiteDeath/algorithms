package com.github.artemkorsakov.tests.matrix

import com.github.artemkorsakov.matrix.HadamardMatrix
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers._

class HadamardMatrixSuite extends AnyFunSuiteLike {
  test("isSymmetrical") {
    assertThrows[IllegalArgumentException] {
      HadamardMatrix(Seq(Seq(1, 0), Seq(1, -1)))
    }
    HadamardMatrix(Seq(Seq(1, 1), Seq(1, -1))).isHadamardMatrix shouldBe true
    HadamardMatrix(
      Seq(Seq(1, 1, 1, 1), Seq(1, -1, 1, -1), Seq(1, 1, -1, -1), Seq(1, -1, -1, 1))
    ).isHadamardMatrix shouldBe true
  }

}
