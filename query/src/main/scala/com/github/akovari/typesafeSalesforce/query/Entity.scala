package com.github.akovari.typesafeSalesforce.query

case class Entity(name: String) extends QueryStringProvider {
  override def toString: String = name
}
