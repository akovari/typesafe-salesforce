package com.github.akovari.typesafeSalesforce.query

import java.util.Calendar

import com.github.akovari.typesafeSalesforce.cxf.enterprise._
import org.junit.runner.RunWith
import org.specs2.matcher.ThrownMessages
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import shapeless.HNil

/**
 * Created by akovari on 26.4.2015.
 */
@RunWith(classOf[JUnitRunner])
class SelectQuerySpec extends Specification with ThrownMessages {
  "select query" should {
    "work with a set of string columns" in {
      import com.github.akovari.typesafeSalesforce.query.SelectQuery._
      import com.github.akovari.typesafeSalesforce.query._

      val cal = Calendar.getInstance()
      val q = select(Case_.caseNumber :: Case_.`engineeringReqNumberC` :: HNil) from Case_ where ((5 :<= 7) and (Case_.caseNumber :== "5555") and (Case_.`createdDate` :== cal))

      q.toString shouldEqual s"SELECT CaseNumber, EngineeringReqNumber__c FROM Case WHERE ((('5' <= '7') AND (CaseNumber = '5555')) AND (CreatedDate = ${CalendarField(cal).toString}))"
    }

    "work with an embedded query" in {
      import com.github.akovari.typesafeSalesforce.query.SelectQuery._
      import com.github.akovari.typesafeSalesforce.query._

      val q = select(Case_.caseNumber :: (select(User_.`contactId` :: HNil) from (Case_.`asset`)) :: HNil) from Case_

      q.toString shouldEqual "SELECT CaseNumber, (SELECT ContactId FROM Asset) FROM Case "
    }

    "work with a nested column" in {
      import com.github.akovari.typesafeSalesforce.query.SelectQuery._
      import com.github.akovari.typesafeSalesforce.query._

      val q = select(Case_.caseNumber :: (Case_.account :/ Account_.accountNumber) :: HNil) from Case_

      q.toString shouldEqual "SELECT CaseNumber, Account.AccountNumber FROM Case "
    }

    "work with an order by" in {
      import com.github.akovari.typesafeSalesforce.query.SelectQuery._
      import com.github.akovari.typesafeSalesforce.query._

      val q = select(Case_.caseNumber :: (Case_.account :/ Account_.accountNumber) :: HNil) from Case_ orderBy (Case_.caseNumber asc)

      q.toString shouldEqual "SELECT CaseNumber, Account.AccountNumber FROM Case ORDER BY CaseNumber ASC "
    }

    "work with multiple order by-s" in {
      import com.github.akovari.typesafeSalesforce.query.SelectQuery._
      import com.github.akovari.typesafeSalesforce.query._

      val q = select(Case_.caseNumber :: (Case_.account :/ Account_.accountNumber) :: HNil) from Case_ orderBy (Case_.caseNumber asc, Case_.`accountId` desc)

      q.toString shouldEqual "SELECT CaseNumber, Account.AccountNumber FROM Case ORDER BY CaseNumber ASC, AccountId DESC "
    }
  }

  "simple condition" should {
    "work with an integer" in {
      import com.github.akovari.typesafeSalesforce.query._

      (5 :== 7).toString shouldEqual "('5' = '7')"
    }

    "work with Boolean" in {
      import com.github.akovari.typesafeSalesforce.query._

      (Case_.`isDeleted` :== true).toString shouldEqual "(IsDeleted = true)"
    }

    "work with in condition" in {
      import com.github.akovari.typesafeSalesforce.query._

      (5 in Seq(Field(5), Field(7))).toString shouldEqual "'5'  IN ('5','7')"
    }
  }
}
