package com.github.artemkorsakov.matrix

import com.github.artemkorsakov.matrix.GenericOperation._

case class MatrixLine[T](row: Seq[T]) {
  def *(col: Seq[T]): T = {
    require(row.length == col.length)
    row.indices.foldLeft(zeroT(row.head))((s, i) => addT(s, mulT(row(i), col(i))))
  }

  def *(col: Seq[T], module: T): T = {
    require(row.length == col.length)
    row.indices.foldLeft(zeroT(row.head))((s, i) => modT(addT(s, mulT(row(i), col(i))), module))
  }

  def *(matrix: Matrix[T]): Seq[T] =
    rowToMatrix.*(matrix).elements.head

  def *(matrix: Matrix[T], module: T): Seq[T] =
    rowToMatrix.*(matrix, module).elements.head

  def rowToMatrix: Matrix[T] = new Matrix[T](Seq(row))

  def columnToMatrix: Matrix[T] = new Matrix[T](row.map(Seq(_)))
}

object MatrixLine {
  implicit def seq2MatrixLine[T](x: Seq[T]): MatrixLine[T] = MatrixLine[T](x)
}
