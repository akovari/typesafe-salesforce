package com.github.akovari.typesafeSalesforce.query

import com.github.akovari.typesafeSalesforce.model.{Field => ModelField}

sealed abstract class Order[T](column: SimpleColumn[T]) extends QueryStringProvider {
  protected val operation: String

  override def toString = column.toString + " " + operation
}

case class AscendingOrder[T](column: SimpleColumn[T]) extends Order(column) {
  override val operation = "ASC"
}

case class DescendingOrder[T](column: SimpleColumn[T]) extends Order(column) {
  override val operation = "DESC"
}
