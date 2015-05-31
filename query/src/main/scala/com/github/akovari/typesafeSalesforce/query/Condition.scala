package com.github.akovari.typesafeSalesforce.query

sealed abstract class Condition(left: Field, right: Field) extends QueryStringProvider {
  protected val operator: String = ""

  override def toString: String = {
    "(" + left.toString + " " + operator + " " + right.toString + ")"
  }

  def and(o: Condition) = AndFilter(this, o)
  def or(o: Condition) = OrFilter(this, o)

  def and(o: Filter) = AndFilter(ConditionFilter(this), o)
  def or(o: Filter) = OrFilter(ConditionFilter(this), o)
}

case class BetweenCondition(left: Field, right: Field, righField: Field) extends Condition(left, right) {
  override def toString: String = {
    "(" + left.toString + " >= " + right.toString + " AND " + left.toString + " <= " + righField.toString + ")"
  }
}

case class EqualsCondition(left: Field, right: Field) extends Condition(left, right) {
  override val operator = "="
}

case class GreaterCondition(left: Field, right: Field) extends Condition(left, right) {
  override val operator = ">"
}

case class GreaterOrEqualsCondition(left: Field, right: Field) extends Condition(left, right) {
  override val operator = ">="
}

case class LowerCondition(left: Field, right: Field) extends Condition(left, right) {
  override val operator = "<"
}

case class LowerOrEqualsCondition(left: Field, right: Field) extends Condition(left, right) {
  override val operator = "<="
}

case class NotEqualsCondition(left: Field, right: Field) extends Condition(left, right) {
  override val operator = "!="
}

case class LikeCondition(left: Field, right: Field) extends Condition(left, right) {
  override val operator = " LIKE "
}

sealed abstract class CollectionCondition(left: Field, right: Field) extends Condition(left, right) {
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

case class ExcludesCondition(left: Field, right: Field) extends CollectionCondition(left, right) {
  override val operator = " EXCLUDES "
}

case class IncludesCondition(left: Field, right: Field) extends CollectionCondition(left, right) {
  override val operator = " INCLUDES "
}

case class InCondition(left: Field, right: Field) extends CollectionCondition(left, right) {
  override val operator = " IN "
}

case class NotInCondition(left: Field, right: Field) extends CollectionCondition(left, right) {
  override val operator = " NOT IN "
}
