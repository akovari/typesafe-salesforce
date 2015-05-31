package com.github.akovari.typesafeSalesforce

import com.github.akovari.typesafeSalesforce.cxf.enterprise.{SObject, QueryResult}
import com.github.akovari.typesafeSalesforce.query.SelectQuery.{QueryStep, SelectableColumn}

import scala.collection.GenSeq
import com.github.akovari.typesafeSalesforce.model.{Field => ModelField, MetaModel}
import scala.reflect.runtime.universe._
import scala.collection.JavaConverters._

/**
 * Created by akovari on 03.11.14.
 */
package object query {
  implicit def fieldToSelectableColumn[T](col: ModelField[T]): SelectableColumn = Left(SimpleColumn(col.name))

  implicit def columnToSelectableColumn(col: SimpleColumn): SelectableColumn = Left(col)

  implicit def queryStepToSelectableColumn(qs: QueryStep): SelectableColumn = Right(qs.query)

  implicit def queryStepToSelectQuery(qs: QueryStep): SelectQuery = qs.query

  implicit def stringToEntity(ent: String): Entity = Entity(ent)

  implicit def fieldToEntity(ent: ModelField[QueryResult]): Entity = Entity(ent.name)

  implicit def fieldToColumn[T](col: ModelField[T]): SimpleColumn = SimpleColumn(col.name)

  implicit def intToField(v: Int): IntegerField = IntegerField(v)

  implicit def doubleToField(v: Double): DoubleField = DoubleField(v)

  implicit def booleanToField(v: java.lang.Boolean): BooleanField = BooleanField(v)

  implicit def scalaBooleanToField(v: Boolean): BooleanField = BooleanField(v)

  implicit def nothingToField(v: Nothing): NullField.type = NullField

  implicit def stringToField(v: String): StringField = StringField(v)

  implicit def queryToColumn(v: SelectQuery): EmbeddedSelectColumn = EmbeddedSelectColumn(v)

  implicit def stringToGroupBy(v: String): GroupBy = GroupBy(v)

  implicit def collectionToCollectionField[T](v: GenSeq[Field]): CollectionField = CollectionField(v)

  implicit def intToLimit(v: Int): Limit = Limit(v)

  implicit def conditionToFilter(cond: Condition): ConditionFilter = ConditionFilter(cond)

  implicit def stringCollectionToFieldCollection[T](v: GenSeq[String]): CollectionField = CollectionField(v.map(v => StringField(v)))

  implicit def intCollectionToFieldCollection[T](v: GenSeq[Int]): CollectionField = CollectionField(v.map(v => IntegerField(v)))

  implicit def doubleCollectionToFieldCollection[T](v: GenSeq[Double]): CollectionField = CollectionField(v.map(v => DoubleField(v)))

  implicit def booleanCollectionToFieldCollection[T](v: GenSeq[Boolean]): CollectionField = CollectionField(v.map(v => BooleanField(v)))

  implicit def metaModelToEntity[T <: SObject](m: MetaModel[T]): Entity = Entity(m.sfEntityName)

  implicit def sfEntityToMetaModel[T <: SObject](implicit t: TypeTag[T]): MetaModel[T] = {
    val mirror = runtimeMirror(getClass.getClassLoader)
    val module = mirror.staticModule(typeOf[T].typeSymbol.fullName + "_").asModule
    mirror.reflectModule(module).instance.asInstanceOf[MetaModel[T]]
  }

  implicit def fieldOfEntityToEntity[T <: SObject](f: ModelField[T])(implicit t: TypeTag[T]): Entity = Entity(sfEntityToMetaModel[T].sfEntityName)
}
