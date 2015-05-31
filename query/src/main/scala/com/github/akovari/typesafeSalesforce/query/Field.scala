package com.github.akovari.typesafeSalesforce.query

import java.text.SimpleDateFormat
import java.util.{Date, TimeZone}

import scala.collection.GenSeq

sealed trait Field extends QueryStringProvider {
  def :==(o: Field) = EqualsCondition(this, o)

  def :!=(o: Field) = NotEqualsCondition(this, o)

  def :<=(o: Field) = LowerOrEqualsCondition(this, o)

  def :>=(o: Field) = GreaterOrEqualsCondition(this, o)

  def :<(o: Field) = LowerCondition(this, o)

  def :>(o: Field) = GreaterCondition(this, o)

  def between(l: Field, r: Field) = BetweenCondition(this, l, r)

  def like(o: Field) = LikeCondition(this, o)

  def in(o: Field) = InCondition(this, o)

  def notIn(o: Field) = NotInCondition(this, o)

  def includes(o: Field) = IncludesCondition(this, o)

  def excludes(o: Field) = ExcludesCondition(this, o)
}

sealed abstract class Column(name: String) extends Field {
  override def toString = name
}

object AggregatedColumnType extends Enumeration {
  type AggregatedColumnType = Value
  val MAX, MIN, SUM = Value
}

case class AggregatedColumn(name: String, `type`: AggregatedColumnType.Value) extends Column(name) {
  override def toString: String = `type`.toString + "(" + super.toString + ")"
}

case class GroupBy(name: String) extends QueryStringProvider

case class SimpleColumn(name: String) extends Column(name) {
  override def toString = name

  def /[TO](o: SimpleColumn): SimpleColumn = SimpleColumn(s"${this.name}.${o.name}")
}

case class EmbeddedSelectColumn(q: SelectQuery) extends Column(q.toString) {
  override def toString = "(" + q.toString.trim + ")"
}

case class CollectionField(seq: GenSeq[Field]) extends Field {
  override def toString = "(" + seq.mkString(",") + ")"
}

case class DoubleField(value: Double) extends Field {
  override def toString = String.valueOf(value)
}

case class IntegerField(value: Int) extends Field {
  override def toString = "'" + String.valueOf(value) + "'"
}

case object NullField extends Field {
  override def toString = "NULL"
}

case class StringField(value: String) extends Field {
  override def toString = "'" + value + "'"
}

case class BooleanField(value: Boolean) extends Field {
  override def toString = value.toString
}

case class DateField(value: Date, timeZone: TimeZone) extends Field {
  private final val simpleDateFormat: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ")
  simpleDateFormat.setTimeZone(timeZone)

  override def toString: String = {
    val date: String = simpleDateFormat.format(value)
    date.substring(0, date.length - 2) + ":" + date.substring(date.length - 2, date.length)
  }
}

object Field {
  val Null = NullField

  def apply(v: String) = StringField(v)

  def apply(v: Int) = IntegerField(v)

  def apply(v: Double) = DoubleField(v)

  def apply(v: Boolean) = BooleanField(v)

  def apply(v: Date, tz: Option[TimeZone] = None) = DateField(v, tz.getOrElse(TimeZone.getDefault))

  def apply(v: GenSeq[Field]) = CollectionField(v)
}
