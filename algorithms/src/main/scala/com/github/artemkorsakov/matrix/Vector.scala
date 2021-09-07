package com.github.artemkorsakov.matrix

import com.github.artemkorsakov.matrix.GenericOperation._

case class Vector[T](row: Seq[T]) {
  require(row.nonEmpty)
  private lazy val zero = zeroT(row.head)

  lazy val norm: Double = sqrtT(*(row))

  def +(col: Seq[T]): Seq[T] = {
    require(row.length == col.length)
    row.indices.map(i => addT(row(i), col(i)))
  }

  def *(a: T): Seq[T] = row.map(mulT(_, a))

  def *(col: Seq[T]): T = {
    require(row.length == col.length)
    row.indices.foldLeft(zero)((s, i) => addT(s, mulT(row(i), col(i))))
  }

  def *(col: Seq[T], module: T): T = {
    require(row.length == col.length)
    row.indices.foldLeft(zero)((s, i) => modT(addT(s, mulT(row(i), col(i))), module))
  }

  def *(matrix: Matrix[T]): Seq[T] =
    rowToMatrix.*(matrix).elements.head

  def *(matrix: Matrix[T], module: T): Seq[T] =
    rowToMatrix.*(matrix, module).elements.head

  def distance(col: Seq[T]): Double =
    sqrtT(row.indices.foldLeft(zero) { (s, i) =>
      val d = subT(row(i), col(i))
      addT(s, mulT(d, d))
    })

  def cos(col: Seq[T]): Double = {
    val scal = *(col)
    val a    = norm
    val b    = Vector(col).norm
    scal.toString.toDouble / (a * b)
  }

  def isOrthogonal(col: Seq[T]): Boolean = *(col) == zero

  def manhattanMetric(col: Seq[T]): T = {
    require(row.length == col.length)
    row.indices.foldLeft(zero)((s, i) => addT(s, absT(subT(row(i), col(i)))))
  }

  def rowToMatrix: Matrix[T] = new Matrix[T](Seq(row))

  def columnToMatrix: Matrix[T] = new Matrix[T](row.map(Seq(_)))
}

object Vector {
  implicit def seq2MatrixLine[T](x: Seq[T]): Vector[T] = Vector[T](x)
}
