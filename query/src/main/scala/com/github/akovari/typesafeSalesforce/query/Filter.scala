package com.github.akovari.typesafeSalesforce.query

/**
 * User: akovari Date: 11/2/13 Time: 3:54 PM
 */
sealed trait Filter extends QueryStringProvider {
  def and(o: Filter) = AndFilter(this, o)
  def or(o: Filter) = OrFilter(this, o)
}

abstract class BooleanFilter(filter1: Filter, filter2: Filter, moreFilters: Filter*) extends Filter {
  val filters = Seq(filter1, filter2) ++ moreFilters
  val operation: String

  override def toString = "(" + filters.mkString(operation) + ")"
}

case class AndFilter(filter1: Filter, filter2: Filter, moreFilters: Filter*) extends BooleanFilter(filter1, filter2, moreFilters: _*) {
  override val operation = " AND "
}

case class OrFilter(filter1: Filter, filter2: Filter, moreFilters: Filter*) extends BooleanFilter(filter1, filter2, moreFilters: _*) {
  override val operation = " OR "
}

case class ConditionFilter[T](condition: Condition[T]) extends Filter {
  override def toString = condition.toString
}
