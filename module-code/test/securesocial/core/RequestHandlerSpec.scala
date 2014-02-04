package securesocial.core

import play.api.test.{WithApplication, FakeRequest, PlaySpecification}
import play.api.mvc.{Request, Cookie, RequestHeader}

/**
 * Created by erik on 2/4/14.
 */
class RequestHandlerSpec extends PlaySpecification {
  "the RequestHandler" should {
    "be usable via SecureSocial" in  {
      val request:RequestHeader = Request().withCookies(Cookie(Authenticator.cookieName, "somevalue"))
      RequestHandler.authenticatorFromRequest(request)
      true mustEqual false
    }
  }


}
