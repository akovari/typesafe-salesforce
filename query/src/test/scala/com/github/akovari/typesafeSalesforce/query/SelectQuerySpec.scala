package com.github.akovari.typesafeSalesforce.query

import com.github.akovari.typesafeSalesforce.cxf.enterprise._
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

/**
 * Created by akovari on 26.4.2015.
 */
@RunWith(classOf[JUnitRunner])
class SelectQuerySpec extends Specification {
  "select query" should {
    "work with a set of string columns" in {
      import com.github.akovari.typesafeSalesforce.query.SelectQuery._
      import com.github.akovari.typesafeSalesforce.query._

      val q = select(Case_.caseNumber, Case_.`engineeringReqNumberC`) from Case_ where ((5 :<= 7) and (Case_.caseNumber :== "5555") and(Case_.`createdDate` :== 12))

      q.toString shouldEqual "SELECT CaseNumber, EngineeringReqNumber__c FROM Case WHERE ((('5' <= '7') AND (CaseNumber = '5555')) AND (CreatedDate = '12'))"
    }

    "work with an embedded query" in {
      import com.github.akovari.typesafeSalesforce.query.SelectQuery._
      import com.github.akovari.typesafeSalesforce.query._

      val q = select(Case_.caseNumber, select(User_.`contactId`) from (Case_.`asset`)) from Case_

      q.toString shouldEqual "SELECT CaseNumber, (SELECT ContactId FROM Asset) FROM Case "
    }

    "work with a nested column" in {
      import com.github.akovari.typesafeSalesforce.query.SelectQuery._
      import com.github.akovari.typesafeSalesforce.query._

      5 :== "test"

      val q = select(Case_.caseNumber, Case_.account / Account_.accountNumber) from Case_

      q.toString shouldEqual "SELECT CaseNumber, Account.AccountNumber FROM Case "
    }
  }

  "simple condition" should {
    "work with an integer" in {
      import com.github.akovari.typesafeSalesforce.query._

      (5 :== 7).toString shouldEqual "('5' = '7')"
    }

    "work with Boolean" in {
      import com.github.akovari.typesafeSalesforce.query._

      (Case_.`reason` :== true).toString shouldEqual "(Reason = true)"
    }

    "work with in condition" in {
      import com.github.akovari.typesafeSalesforce.query._

      (5 in Seq(Field(5), Field(7))).toString shouldEqual "'5'  IN ('5','7')"
    }
  }
}
