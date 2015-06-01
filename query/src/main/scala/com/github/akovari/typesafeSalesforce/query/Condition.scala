package com.github.akovari.typesafeSalesforce.query

sealed abstract class Condition[T](left: Field[T], right: Field[T]) extends QueryStringProvider {
  protected val operator: String

  override def toString: String = {
    "(" + left.toString + " " + operator + " " + right.toString + ")"
  }

  def and[TO](o: Condition[TO]) = AndFilter(this, o)
  def or[TO](o: Condition[TO]) = OrFilter(this, o)

  def and(o: Filter) = AndFilter(ConditionFilter(this), o)
  def or(o: Filter) = OrFilter(ConditionFilter(this), o)
}

case class BetweenCondition[T](left: Field[T], right: Field[T], rightField: Field[T]) extends Condition(left, right) {
  override val operator = null

  override def toString: String = {
    "(" + left.toString + " >= " + right.toString + " AND " + left.toString + " <= " + rightField.toString + ")"
  }
}

case class EqualsCondition[T](left: Field[T], right: Field[T]) extends Condition(left, right) {
  override val operator = "="
}

case class GreaterCondition[T](left: Field[T], right: Field[T]) extends Condition(left, right) {
  override val operator = ">"
}

case class GreaterOrEqualsCondition[T](left: Field[T], right: Field[T]) extends Condition(left, right) {
  override val operator = ">="
}

case class LowerCondition[T](left: Field[T], right: Field[T]) extends Condition(left, right) {
  override val operator = "<"
}

case class LowerOrEqualsCondition[T](left: Field[T], right: Field[T]) extends Condition(left, right) {
  override val operator = "<="
}

case class NotEqualsCondition[T](left: Field[T], right: Field[T]) extends Condition(left, right) {
  override val operator = "!="
}

case class LikeCondition[T](left: Field[T], right: Field[T]) extends Condition(left, right) {
  override val operator = " LIKE "
}

sealed abstract class CollectionCondition[T](left: Field[T], right: Field[T]) extends Condition(left, right) {
  override def toString: String = {
    val sb = new StringBuilder
    sb.append(left.toString)
    sb.append(' ')
    sb.append(operator)
    val rightPart = right.toString
    if (!rightPart.startsWith("(")) {
      sb.append('(')
    }
    sb.append(right.toString)
    if (!rightPart.endsWith(")")) {
      sb.append(')')
    }
    sb.toString()
  }
}

case class ExcludesCondition[T](left: CollectionField[T], right: Field[T]) extends CollectionCondition(left, right) {
  override val operator = " EXCLUDES "
}

case class IncludesCondition[T](left: CollectionField[T], right: Field[T]) extends CollectionCondition(left, right) {
  override val operator = " INCLUDES "
}

case class InCondition[T](left: Field[T], right: CollectionField[T]) extends CollectionCondition(left, right) {
  override val operator = " IN "
}

case class NotInCondition[T](left: Field[T], right: CollectionField[T]) extends CollectionCondition(left, right) {
  override val operator = " NOT IN "
}
