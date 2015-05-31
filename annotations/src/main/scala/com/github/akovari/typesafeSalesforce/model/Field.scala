package com.github.akovari.typesafeSalesforce.model

import scala.reflect.runtime.universe._

/**
 * Created by akovari on 25.4.2015.
 */
case class Field[T: TypeTag](name: String) {
  val tpe = typeOf[T]
}
