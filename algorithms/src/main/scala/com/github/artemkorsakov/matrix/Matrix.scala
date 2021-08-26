package com.github.artemkorsakov.matrix

import com.github.artemkorsakov.matrix.GenericOperation._
import com.github.artemkorsakov.matrix.Matrix.seq2Matrix
import com.github.artemkorsakov.matrix.MatrixLine.seq2MatrixLine

/** Matrix m * n.
  *
  * @see <a href="https://en.wikipedia.org/wiki/Matrix_(mathematics)">detailed description</a>
  */
case class Matrix[T](elements: Seq[Seq[T]]) {

  /** Row's count. */
  val m: Int = elements.length
  require(m > 0)

  /** Column's count. */
  val n: Int = elements.head.length
  require((0 until m).forall(i => elements(i).length == n))

  def row(i: Int): Seq[T] = elements(i)

  def column(j: Int): Seq[T] = (0 until m).map(i => elements(i)(j))

  /** <a href="https://en.wikipedia.org/wiki/Transpose">Transpose</a> of a matrix. */
  def transpose: Matrix[T] =
    Matrix((0 until n).map(j => (0 until m).map(i => elements(i)(j))))

  /** New matrix without the given row and the given column. */
  def minor(row: Int, column: Int): Matrix[T] =
    Matrix(
      (0 until m)
        .withFilter(_ != row)
        .map(i => (0 until n).withFilter(_ != column).map(j => elements(i)(j)))
    )

  /** <a href="https://en.wikipedia.org/wiki/Determinant">Determinant</a> of a matrix. */
  def determinant: T =
    if (elements.length == 1) {
      elements.head.head
    } else {
      (0 until n).foldLeft(zeroT(elements.head.head)) { (sum, i) =>
        val mul = mulT(elements.head(i), elements.minor(0, i).determinant)
        if (i % 2 == 0) addT(sum, mul) else subT(sum, mul)
      }
    }

  def isTheSameSize(other: Matrix[T]): Boolean = m == other.m && n == other.n

  def +(other: Matrix[T]): Matrix[T] = {
    require(isTheSameSize(other))
    Matrix((0 until m).map(i => (0 until n).map(j => addT(elements(i)(j), other.elements(i)(j)))))
  }

  def *(c: T): Matrix[T] =
    Matrix((0 until m).map(i => (0 until n).map(j => mulT(elements(i)(j), c))))

  def *(c: T, module: T): Matrix[T] =
    Matrix((0 until m).map(i => (0 until n).map(j => modT(mulT(elements(i)(j), c), module))))

  /** <a href="https://en.wikipedia.org/wiki/Matrix_multiplication">multiplication</a> */
  def *(other: Matrix[T]): Matrix[T] = {
    require(n == other.m)
    val newElements = (0 until m)
      .map(i => (0 until other.n).map(j => elements(i).*((0 until n).map(k => other.elements(k)(j)))))
    Matrix(newElements)
  }

  def *(other: Matrix[T], module: T): Matrix[T] = {
    require(n == other.m)
    val newElements = (0 until m)
      .map(i => (0 until other.n).map(j => elements(i).*((0 until n).map(k => other.elements(k)(j)), module)))
    Matrix(newElements)
  }

  def *(other: MatrixLine[T]): MatrixLine[T] =
    *(other.columnToMatrix).elements.map(_.head)

  def *(other: MatrixLine[T], module: T): MatrixLine[T] =
    *(other.columnToMatrix, module).elements.map(_.head)

  /** Matrix exponentiation. */
  def power(p: Long): Matrix[T] = {
    require(p >= 1)
    if (p == 1) {
      this
    } else {
      val powers  = p.toBinaryString
      val powersC = new Array[Matrix[T]](powers.length)
      powersC(0) = this
      (1 until powers.length).foreach(i => powersC(i) = powersC(i - 1) * powersC(i - 1))
      var result = powersC.last
      (1 until powers.length).withFilter(powers(_) == '1').foreach { i =>
        result = result * powersC(powersC.length - 1 - i)
      }
      result
    }
  }

  def power(p: Long, module: T): Matrix[T] = {
    require(p >= 1)
    if (p == 1) {
      Matrix(elements.map(_.map(modT(_, module))))
    } else {
      val powers  = p.toBinaryString
      val powersC = new Array[Matrix[T]](powers.length)
      powersC(0) = this
      (1 until powers.length).foreach(i => powersC(i) = powersC(i - 1) * (powersC(i - 1), module))
      var result = powersC.last
      (1 until powers.length).withFilter(powers(_) == '1').foreach { i =>
        result = result * (powersC(powersC.length - 1 - i), module)
      }
      result
    }
  }

  override def toString: String =
    elements.map(row => row.mkString("| ", ", ", " |")).mkString("\n")
}

/** Matrix.
  *
  * @see <a href="https://en.wikipedia.org/wiki/Matrix_(mathematics)">detailed description</a>
  */
object Matrix {
  implicit def seq2Matrix[T](elements: Seq[Seq[T]]): Matrix[T] = new Matrix[T](elements)
}
