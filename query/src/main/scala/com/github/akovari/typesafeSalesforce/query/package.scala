package com.github.akovari.typesafeSalesforce

import java.util.Calendar

import com.github.akovari.typesafeSalesforce.cxf.enterprise.{QueryResult, SObject}
import com.github.akovari.typesafeSalesforce.model.{Field => ModelField, MetaModel}
import com.github.akovari.typesafeSalesforce.query.SelectQuery._
import shapeless._
import shapeless.ops.hlist._

import scala.collection.GenSeq
import scala.reflect.runtime.universe._

/**
 * Created by akovari on 03.11.14.
 */
package object query {
  implicit def queryStepToSelectableColumn[T, C <: HList, O <: HList](qs: QueryStep[C]): EmbeddedSelectColumn[T, C] = qs.query

  implicit def queryStepToSelectQuery[C <: HList](qs: QueryStep[C]): SelectQuery[C] = qs.query

  implicit def stringToEntity(ent: String): Entity = Entity(ent)

  implicit def fieldToEntity(ent: ModelField[QueryResult]): Entity = Entity(ent.name)

  implicit def fieldToColumn[T](col: ModelField[T]): SimpleColumn[T] = SimpleColumn(col.name)

  implicit def intToField(v: Int): IntegerField = IntegerField(v)

  implicit def doubleToField(v: Double): DoubleField = DoubleField(v)

  implicit def booleanToField(v: java.lang.Boolean): BooleanField = BooleanField(v)

  implicit def scalaBooleanToField(v: Boolean): BooleanField = BooleanField(v)

  implicit def nothingToField(v: Nothing): NullField.type = NullField

  implicit def stringToField(v: String): StringField = StringField(v)

  implicit def calendarToField(v: Calendar): CalendarField = CalendarField(v)

  implicit def queryToColumn[T, C <: HList](v: SelectQuery[C]): EmbeddedSelectColumn[T, C] = EmbeddedSelectColumn(v)

  implicit def stringToGroupBy[T](v: String): GroupBy[T] = GroupBy(v)

  implicit def fieldToGroupBy[T](col: ModelField[T]): GroupBy[T] = GroupBy(col.name)

  implicit def collectionToCollectionField[T](v: GenSeq[Field[T]]): CollectionField[T] = CollectionField(v)

  implicit def intToLimit(v: Int): Limit = Limit(v)

  implicit def conditionToFilter[T](cond: Condition[T]): ConditionFilter[T] = ConditionFilter(cond)

  implicit def stringCollectionToFieldCollection(v: GenSeq[String]): CollectionField[String] = CollectionField(v.map(v => StringField(v)))

  implicit def intCollectionToFieldCollection[T](v: GenSeq[Int]): CollectionField[Int] = CollectionField(v.map(v => IntegerField(v)))

  implicit def doubleCollectionToFieldCollection[T](v: GenSeq[Double]): CollectionField[Double] = CollectionField(v.map(v => DoubleField(v)))

  implicit def booleanCollectionToFieldCollection[T](v: GenSeq[java.lang.Boolean]): CollectionField[java.lang.Boolean] = CollectionField(v.map(v => BooleanField(v)))

  implicit def metaModelToEntity[T <: SObject](m: MetaModel[T]): Entity = Entity(m.sfEntityName)

  implicit def sfEntityToMetaModel[T <: SObject](implicit t: TypeTag[T]): MetaModel[T] = {
    val mirror = runtimeMirror(getClass.getClassLoader)
    val module = mirror.staticModule(typeOf[T].typeSymbol.fullName + "_").asModule
    mirror.reflectModule(module).instance.asInstanceOf[MetaModel[T]]
  }

  implicit def fieldOfEntityToEntity[T <: SObject](f: ModelField[T])(implicit t: TypeTag[T]): Entity = Entity(sfEntityToMetaModel[T].sfEntityName)


  case class ColumnList[L <: HList](l : L)

  object ColumnPoly extends Poly1 {
    implicit def caseField[T, S <% ModelField[T]] = at[ModelField[T]](f => fieldToColumn(f).toString)

    implicit def caseSelectQueryStep[T, C <: HList, QS <: SelectQueryStep[C], S <% QS] = at[SelectQueryStep[C]](qs => EmbeddedSelectColumn[T, C](qs).toString)
    implicit def caseFromQueryStep[T, C <: HList, QS <: FromQueryStep[C], S <% QS] = at[FromQueryStep[C]](qs => EmbeddedSelectColumn[T, C](qs).toString)
    implicit def caseWhereQueryStep[T, C <: HList, QS <: WhereQueryStep[C], S <% QS] = at[WhereQueryStep[C]](qs => EmbeddedSelectColumn[T, C](qs).toString)

    implicit def caseOrderQueryStep[T, C <: HList, O <: HList, QS <: OrderQueryStep[C, O], S <% QS] = at[OrderQueryStep[C, O]](qs => EmbeddedSelectColumn[T, C](qs).toString)
    implicit def caseGroupByQueryStep[T, C <: HList, QS <: GroupByQueryStep[C], S <% QS] = at[GroupByQueryStep[C]](qs => EmbeddedSelectColumn[T, C](qs).toString)
    implicit def caseLimitQueryStep[T, C <: HList, QS <: LimitQueryStep[C], S <% QS] = at[LimitQueryStep[C]](qs => EmbeddedSelectColumn[T, C](qs).toString)

    implicit def caseSimpleColumn[T, SC <: SimpleColumn[T], S <% SC] = at[SimpleColumn[T]](_.toString)
    implicit def caseEmbeddedSelectColumn[T, C <: HList, SC <: EmbeddedSelectColumn[T, C], S <% SC] = at[EmbeddedSelectColumn[T, C]](_.toString)
  }

  implicit def hlistToColumnList[L <: HList, M <: HList](a: L)(implicit mapper: Mapper.Aux[ColumnPoly.type, L, M]): ColumnList[M] =
    new ColumnList[M]((a map ColumnPoly))


  case class OrderList[L <: HList](l: L)

  object OrderPoly extends Poly1 {
    implicit def caseAsc[T, S <% AscendingOrder[T]] = at[AscendingOrder[T]](_.toString)

    implicit def caseDesc[T, S <% DescendingOrder[T]] = at[DescendingOrder[T]](_.toString)
  }

  implicit def hlistToOrderList[L <: HList, M <: HList](a: L)(implicit mapper: Mapper.Aux[OrderPoly.type, L, M]): OrderList[M] =
    new OrderList[M]((a map OrderPoly))
}
