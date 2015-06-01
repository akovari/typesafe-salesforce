package com.github.akovari.typesafeSalesforce.query

import shapeless._

case class SelectQuery[C <: HList](columns: ColumnList[C],
                       entities: Seq[Entity] = Seq.empty,
                       filter: Option[Filter] = None,
                       orders: Seq[Order[_]] = Seq.empty,
                       groupBys: Seq[GroupBy[_]] = Seq.empty,
                       limit: Option[Limit] = None) extends QueryStringProvider {
  override def toString = {
    val sb = new StringBuilder
    sb.append("SELECT ")

    def iterateCols[C <: HList](l: C): String = l match {
      case h :: t => s"${h}, ${iterateCols(t)}"
      case _ => ""
    }

    sb.append(iterateCols(columns.l))
    sb.delete(sb.length - 2, sb.length - 1)
    sb.append("FROM ")
    for (entity <- entities) {
      sb.append(entity.toString)
      sb.append(", ")
    }
    sb.delete(sb.length - 2, sb.length - 1)
    if (filter.isDefined) {
      sb.append("WHERE ")
      sb.append(filter.get.toString)
    }
    if (!groupBys.isEmpty) {
      if (filter.isDefined) {
        sb.append(" ")
      }
      sb.append("GROUP BY ")
      for (groupBy <- groupBys) {
        sb.append(groupBy.toString)
        sb.append(", ")
      }
      sb.delete(sb.length - 2, sb.length - 1)
    }
    if (!orders.isEmpty) {
      if (filter.isDefined) {
        sb.append(" ")
      }
      sb.append("ORDER BY ")
      for (order <- orders) {
        sb.append(order.toString)
        sb.append(", ")
      }
      sb.delete(sb.length - 2, sb.length - 1)
    }
    if (limit.isDefined) {
      if (filter.isDefined && orders.isEmpty) {
        sb.append(" ")
      }
      sb.append("LIMIT " + limit.get.value)
    }
    sb.toString()
  }
}

object SelectQuery {
  type SelectableColumn[T, C <: HList] = Either[SimpleColumn[T], EmbeddedSelectColumn[T, C]]

  def select[C <: HList](columns: ColumnList[C]) = SelectQueryStep(columns)

  sealed trait QueryStep[C <: HList] {
    val query: SelectQuery[C]

    override def toString = query.toString
  }

  case class SelectQueryStep[C <: HList](columns: ColumnList[C]) extends QueryStep[C] {
    override val query = SelectQuery(columns = columns)

    def from(entities: Entity*): FromQueryStep[C] = FromQueryStep(query, entities)
  }

  case class FromQueryStep[C <: HList](sq: SelectQuery[C], entities: Seq[Entity]) extends QueryStep[C] {
    override val query = SelectQuery(columns = sq.columns, entities = entities)

    def where(filter: Filter): WhereQueryStep[C] = WhereQueryStep(query, filter)

    def orderBy(orders: Order[_]*): OrderQueryStep[C] = OrderQueryStep(query, orders)

    def groupBy(groupBys: GroupBy[_]*): GroupByQueryStep[C] = GroupByQueryStep(query, groupBys)

    def limit(limit: Limit): LimitQueryStep[C] = LimitQueryStep(query, limit)
  }

  case class WhereQueryStep[C <: HList](sq: SelectQuery[C], filter: Filter) extends QueryStep[C] {
    override val query = SelectQuery(columns = sq.columns, entities = sq.entities, filter = Some(filter))

    def orderBy(orders: Order[_]*): OrderQueryStep[C] = OrderQueryStep(query, orders)

    def groupBy(groupBys: GroupBy[_]*): GroupByQueryStep[C] = GroupByQueryStep(query, groupBys)

    def limit(limit: Limit): LimitQueryStep[C] = LimitQueryStep(query, limit)
  }

  case class OrderQueryStep[C <: HList](sq: SelectQuery[C], orders: Seq[Order[_]]) extends QueryStep[C] {
    override val query = SelectQuery(columns = sq.columns, entities = sq.entities, filter = sq.filter, orders = orders)

    def groupBy(groupBys: GroupBy[_]*): GroupByQueryStep[C] = GroupByQueryStep(query, groupBys)

    def limit(limit: Limit): LimitQueryStep[C] = LimitQueryStep(query, limit)
  }

  case class GroupByQueryStep[C <: HList](sq: SelectQuery[C], groupBys: Seq[GroupBy[_]]) extends QueryStep[C] {
    override val query = SelectQuery(columns = sq.columns, entities = sq.entities, filter = sq.filter, orders = sq.orders, groupBys = groupBys)

    def limit(limit: Limit): LimitQueryStep[C] = LimitQueryStep(query, limit)
  }

  case class LimitQueryStep[C <: HList](sq: SelectQuery[C], limit: Limit) extends QueryStep[C] {
    override val query = SelectQuery(columns = sq.columns, entities = sq.entities, filter = sq.filter, orders = sq.orders, groupBys = sq.groupBys, limit = Some(limit))
  }
}
