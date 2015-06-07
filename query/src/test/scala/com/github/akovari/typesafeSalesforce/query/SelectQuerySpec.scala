package com.github.akovari.typesafeSalesforce.query

import java.util.Calendar

import com.github.akovari.typesafeSalesforce.cxf.enterprise._
import org.scalatest._
import shapeless.HNil

/**
 * Created by akovari on 26.4.2015.
 */
class SelectQuerySpec extends FlatSpec with Matchers {
  "select query" should "work with a set of string columns" in {
    import com.github.akovari.typesafeSalesforce.query.SelectQuery._
    import com.github.akovari.typesafeSalesforce.query._

    val cal = Calendar.getInstance()
    val q = select(Case_.caseNumber :: Case_.`engineeringReqNumberC` :: HNil) from Case_ where ((5 :<= 7) and (Case_.caseNumber :== "5555") and (Case_.`createdDate` :== cal))

    q.toString shouldEqual s"SELECT CaseNumber, EngineeringReqNumber__c FROM Case WHERE ((('5' <= '7') AND (CaseNumber = '5555')) AND (CreatedDate = ${CalendarField(cal).toString}))"
  }

  "select query" should "work with an embedded query" in {
    import com.github.akovari.typesafeSalesforce.query.SelectQuery._
    import com.github.akovari.typesafeSalesforce.query._

    val q = select(Case_.caseNumber :: (select(User_.`contactId` :: HNil) from (Case_.`asset`)) :: HNil) from Case_

    q.toString shouldEqual "SELECT CaseNumber, (SELECT ContactId FROM Asset) FROM Case "
  }

  "select query" should "work with a nested column" in {
    import com.github.akovari.typesafeSalesforce.query.SelectQuery._
    import com.github.akovari.typesafeSalesforce.query._

    val q = select(Case_.caseNumber :: (Case_.account :/ Account_.accountNumber) :: HNil) from Case_

    q.toString shouldEqual "SELECT CaseNumber, Account.AccountNumber FROM Case "
  }

  "select query" should "work with an order by" in {
    import com.github.akovari.typesafeSalesforce.query.SelectQuery._
    import com.github.akovari.typesafeSalesforce.query._

    val q = select(Case_.caseNumber :: HNil) from Case_ orderBy ((Case_.caseNumber asc) :: HNil)

    q.toString shouldEqual "SELECT CaseNumber FROM Case ORDER BY CaseNumber ASC "
  }

  "select query" should "work with multiple order by-s" in {
    import com.github.akovari.typesafeSalesforce.query.SelectQuery._
    import com.github.akovari.typesafeSalesforce.query._

    val q = select(Case_.caseNumber :: (Case_.account :/ Account_.accountNumber) :: HNil) from Case_ orderBy ((Case_.caseNumber asc) :: (Case_.`accountId` desc) :: HNil)

    q.toString shouldEqual "SELECT CaseNumber, Account.AccountNumber FROM Case ORDER BY CaseNumber ASC, AccountId DESC "
  }

  "select query" should "work with an order by and group by" in {
    import com.github.akovari.typesafeSalesforce.query.SelectQuery._
    import com.github.akovari.typesafeSalesforce.query._

    val q = select(Case_.caseNumber :: HNil) from Case_ orderBy ((Case_.caseNumber asc) :: HNil) groupBy(SObject_.id)

    q.toString shouldEqual "SELECT CaseNumber FROM Case GROUP BY Id ORDER BY CaseNumber ASC "
  }

  "select query" should "work with multiple group by-s" in {
    import com.github.akovari.typesafeSalesforce.query.SelectQuery._
    import com.github.akovari.typesafeSalesforce.query._

    val q = select(Case_.caseNumber :: HNil) from Case_ groupBy(SObject_.id, Case_.caseNumber)

    q.toString shouldEqual "SELECT CaseNumber FROM Case GROUP BY Id, CaseNumber "
  }

  "simple condition" should "work with an integer" in {

    import com.github.akovari.typesafeSalesforce.query._

    (5 :== 7).toString shouldEqual "('5' = '7')"
  }

  "simple condition" should "work with Boolean" in {
    import com.github.akovari.typesafeSalesforce.query._

    (Case_.`isDeleted` :== true).toString shouldEqual "(IsDeleted = true)"
  }

  "simple condition" should "work with in condition" in {
    import com.github.akovari.typesafeSalesforce.query._

    (5 in Seq(Field(5), Field(7))).toString shouldEqual "'5'  IN ('5','7')"
  }
}
