package com.github.akovari.typesafeSalesforce.query

sealed abstract class Order(column: Column) extends QueryStringProvider {
  protected val operation: String

  override def toString = column.toString + " " + operation
}

case class AscendingOrder(column: Column) extends Order(column) {
  override val operation = "ASC"
}

case class DescendingOrder(column: Column) extends Order(column) {
  override val operation = "DESC"
}
