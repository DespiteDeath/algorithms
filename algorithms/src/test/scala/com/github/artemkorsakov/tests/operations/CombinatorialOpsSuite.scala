package com.github.artemkorsakov.tests.operations

import com.github.artemkorsakov.operations.CombinatorialOps._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers._

class CombinatorialOpsSuite extends AnyFunSuiteLike {
  test("xcombinations") {
    List("a", "b", "c").xcombinations(2) shouldBe List(List("a", "b"), List("a", "c"), List("b", "c"))
  }

  test("xsubsets") {
    List("a", "b", "c").xsubsets shouldBe List(
      List("a", "b", "c"),
      List("a", "b"),
      List("a", "c"),
      List("b", "c"),
      List("a"),
      List("b"),
      List("c")
    )
  }

  test("xvariations") {
    List("a", "b", "c").xvariations(2) shouldBe List(
      List("b", "a"),
      List("a", "b"),
      List("c", "a"),
      List("a", "c"),
      List("c", "b"),
      List("b", "c")
    )
  }

  test("xpermutations") {
    List("a", "b", "c").xpermutations shouldBe List(
      List("c", "b", "a"),
      List("c", "a", "b"),
      List("a", "c", "b"),
      List("b", "c", "a"),
      List("b", "a", "c"),
      List("a", "b", "c")
    )
  }
}
