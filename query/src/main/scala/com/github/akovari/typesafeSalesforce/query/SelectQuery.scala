package com.github.akovari.typesafeSalesforce.query

import com.github.akovari.typesafeSalesforce.query.SelectQuery.SelectableColumn

case class SelectQuery(columns: Seq[SelectableColumn] = Seq.empty,
                       entities: Seq[Entity] = Seq.empty,
                       filter: Option[Filter] = None,
                       orders: Seq[Order] = Seq.empty,
                       groupBys: Seq[GroupBy] = Seq.empty,
                       limit: Option[Limit] = None) extends QueryStringProvider {
  override def toString = {
    val sb = new StringBuilder
    sb.append("SELECT ")
    for (column <- columns) {
      sb.append(column.fold(_.toString, _.toString))
      sb.append(", ")
    }
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
  type SelectableColumn = Either[SimpleColumn, EmbeddedSelectColumn]

  def select(columns: SelectableColumn*) = SelectQueryStep(columns)

  sealed trait QueryStep {
    val query: SelectQuery

    override def toString = query.toString
  }

  case class SelectQueryStep(columns: Seq[SelectableColumn]) extends QueryStep {
    override val query = SelectQuery(columns = columns)

    def from(entities: Entity*): FromQueryStep = FromQueryStep(query, entities)
  }

  case class FromQueryStep(sq: SelectQuery, entities: Seq[Entity]) extends QueryStep {
    override val query = SelectQuery(columns = sq.columns, entities = entities)

    def where(filter: Filter): WhereQueryStep = WhereQueryStep(query, filter)

    def orderBy(orders: Order*): OrderQueryStep = OrderQueryStep(query, orders)

    def groupBy(groupBys: GroupBy*): GroupByQueryStep = GroupByQueryStep(query, groupBys)

    def limit(limit: Limit): LimitQueryStep = LimitQueryStep(query, limit)
  }

  case class WhereQueryStep(sq: SelectQuery, filter: Filter) extends QueryStep {
    override val query = SelectQuery(columns = sq.columns, entities = sq.entities, filter = Some(filter))

    def orderBy(orders: Order*): OrderQueryStep = OrderQueryStep(query, orders)

    def groupBy(groupBys: GroupBy*): GroupByQueryStep = GroupByQueryStep(query, groupBys)

    def limit(limit: Limit): LimitQueryStep = LimitQueryStep(query, limit)
  }

  case class OrderQueryStep(sq: SelectQuery, orders: Seq[Order]) extends QueryStep {
    override val query = SelectQuery(columns = sq.columns, entities = sq.entities, filter = sq.filter, orders = orders)

    def groupBy(groupBys: GroupBy*): GroupByQueryStep = GroupByQueryStep(query, groupBys)

    def limit(limit: Limit): LimitQueryStep = LimitQueryStep(query, limit)
  }

  case class GroupByQueryStep(sq: SelectQuery, groupBys: Seq[GroupBy]) extends QueryStep {
    override val query = SelectQuery(columns = sq.columns, entities = sq.entities, filter = sq.filter, orders = sq.orders, groupBys = groupBys)

    def limit(limit: Limit): LimitQueryStep = LimitQueryStep(query, limit)
  }

  case class LimitQueryStep(sq: SelectQuery, limit: Limit) extends QueryStep {
    override val query = SelectQuery(columns = sq.columns, entities = sq.entities, filter = sq.filter, orders = sq.orders, groupBys = sq.groupBys, limit = Some(limit))
  }
}
