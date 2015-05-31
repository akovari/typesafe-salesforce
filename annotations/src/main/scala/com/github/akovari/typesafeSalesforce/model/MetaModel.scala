package com.github.akovari.typesafeSalesforce.model

import scala.reflect.runtime.universe._

/**
 * Created by akovari on 26.4.2015.
 */
trait MetaModel[T] {
  val sfEntityName: String
  val sfEntityType: TypeTag[T]
}
