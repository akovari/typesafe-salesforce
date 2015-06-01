package com.github.akovari.typesafeSalesforce.query

import java.text.SimpleDateFormat
import java.util.{Calendar, Date, TimeZone}

import shapeless.HList

import scala.collection.GenSeq

sealed trait Field[+T] extends QueryStringProvider {
  def :==[TO >: T](o: Field[TO]) = EqualsCondition(this, o)

  def :!=[TO >: T](o: Field[TO]) = NotEqualsCondition(this, o)

  def :<=[TO >: T](o: Field[TO]) = LowerOrEqualsCondition(this, o)

  def :>=[TO >: T](o: Field[TO]) = GreaterOrEqualsCondition(this, o)

  def :<[TO >: T](o: Field[TO]) = LowerCondition(this, o)

  def :>[TO >: T](o: Field[TO]) = GreaterCondition(this, o)

  def between[TO >: T](l: Field[TO], r: Field[TO]) = BetweenCondition(this, l, r)

  def like[TO >: T](o: Field[TO]) = LikeCondition(this, o)

  def in[TO >: T](o: CollectionField[TO]) = InCondition(this, o)

  def notIn[TO >: T](o: CollectionField[TO]) = NotInCondition(this, o)
}

sealed abstract class Column[T](name: String) extends Field[T] {
  override def toString = name
}

object AggregatedColumnType extends Enumeration {
  type AggregatedColumnType = Value
  val MAX, MIN, SUM = Value
}

case class AggregatedColumn[T](name: String, `type`: AggregatedColumnType.Value) extends Column[T](name) {
  override def toString: String = `type`.toString + "(" + super.toString + ")"
}

case class GroupBy[T](name: String) extends QueryStringProvider

case class SimpleColumn[T](name: String) extends Column[T](name) {
  override def toString = name

  def :/[TO](o: SimpleColumn[TO]): SimpleColumn[TO] = SimpleColumn(s"${this.name}.${o.name}")

  val asc = AscendingOrder(this)
  val desc = DescendingOrder(this)
}

case class EmbeddedSelectColumn[T, C <: HList](q: SelectQuery[C]) extends Column[T](q.toString) {
  override def toString = "(" + q.toString.trim + ")"
}

case class CollectionField[T](seq: GenSeq[Field[T]]) extends Field[GenSeq[T]] {
  override def toString = "(" + seq.mkString(",") + ")"

  def includes(o: Field[T]) = IncludesCondition(this, o)

  def excludes(o: Field[T]) = ExcludesCondition(this, o)
}

case class DoubleField(value: Double) extends Field[Double] {
  override def toString = String.valueOf(value)
}

case class IntegerField(value: Int) extends Field[Int] {
  override def toString = "'" + String.valueOf(value) + "'"
}

case object NullField extends Field[Null] {
  override def toString = "NULL"
}

case class StringField(value: String) extends Field[String] {
  override def toString = "'" + value + "'"
}

case class BooleanField(value: Boolean) extends Field[Boolean] {
  override def toString = value.toString
}

object DateTimeFields {
  val simpleDateFormat: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ")

  def toString(value: Date, format: SimpleDateFormat): String = {
    val date: String = format.format(value)
    date.substring(0, date.length - 2) + ":" + date.substring(date.length - 2, date.length)
  }
}

case class DateField(value: Date, timeZone: TimeZone) extends Field[(Date, TimeZone)] {
  val simpleDateFormat = DateTimeFields.simpleDateFormat.clone().asInstanceOf[SimpleDateFormat]
  simpleDateFormat.setTimeZone(timeZone)

  override def toString: String = DateTimeFields.toString(value, simpleDateFormat)
}

case class CalendarField(value: Calendar) extends Field[Calendar] {
  override def toString: String = DateTimeFields.toString(value.getTime, DateTimeFields.simpleDateFormat)
}

object Field {
  val Null = NullField

  def apply(v: String) = StringField(v)

  def apply(v: Int) = IntegerField(v)

  def apply(v: Double) = DoubleField(v)

  def apply(v: Boolean) = BooleanField(v)

  def apply(v: Calendar) = CalendarField(v)

  def apply(v: Date, tz: Option[TimeZone] = None) = DateField(v, tz.getOrElse(TimeZone.getDefault))

  def apply[T](v: GenSeq[Field[T]]) = CollectionField(v)
}
