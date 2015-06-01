package com.github.akovari.typesafeSalesforce.util

import com.github.akovari.typesafeSalesforce.cxf.enterprise.{QueryResult, SObject, SforceService, Soap}
import com.github.akovari.typesafeSalesforce.query.SelectQuery
import org.flossware.util.properties.FilePropertiesMgr
import org.solenopsis.lasius.credentials.PropertiesCredentials
import org.solenopsis.lasius.wsimport.util.EnterpriseWebServiceUtil
import shapeless.HList

import scala.async.Async.{async, await}
import scala.collection.JavaConverters._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
 * User: akovari
 * Date: 5/13/14
 * Time: 10:37 AM
 */

case class SalesForceQueryException(query: String, rootCause: Throwable) extends Exception(query, rootCause)

trait SalesForceConversions {
  implicit val port: Future[Soap]

  implicit def queryResultToList[T](qResult: QueryResult)(implicit executionContext: ExecutionContext): Future[Seq[T]] =
    async {
      if (qResult != null && qResult.getSize > 0) {
        val records = qResult.getRecords.asScala.toStream.asInstanceOf[Seq[T]]
        if (qResult.isDone) records
        else records ++ await(queryResultToList(await(port).queryMore(qResult.getQueryLocator)))
      } else {
        Seq.empty[T]
      }
    }
}

trait SalesForceConnection {
  this: SalesForceConversions =>
  implicit val executionContext: ExecutionContext

  implicit val port: Future[Soap] = async(EnterpriseWebServiceUtil.createEnterpriseProxyPort(
    new PropertiesCredentials(new FilePropertiesMgr(getClass.getResource("salesforce.properties").getPath)),
    classOf[SforceService]
  ))

  @throws(classOf[SalesForceQueryException])
  def query[T <: SObject, C <: HList](query: SelectQuery[C])(implicit executionContext: ExecutionContext): Future[Seq[T]] = async {
    val p = await(port)
    Try(p.queryAll(query.toString)) match {
      case Success(r) => await(r)
      case Failure(e) => throw SalesForceQueryException(query.toString, e)
    }
  }
}

class SalesForceConnectionImpl(override implicit val executionContext: ExecutionContext) extends SalesForceConnection with SalesForceConversions
