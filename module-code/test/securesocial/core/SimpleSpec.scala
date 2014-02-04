package securesocial.core

import play.api.test.PlaySpecification

/**
 * Created by erik on 2/3/14.
 */
class SimpleSpec extends PlaySpecification {
  "the simple spec" should {
    "be able to compute trivial arithemtic" in {
      2 + 2 mustEqual 4
    }
  }


}
