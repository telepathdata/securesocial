package securesocial.core

import play.api.test.{WithApplication, PlaySpecification}

/**
 * Created by erik on 2/8/14.
 */
class RequestHandlerSpec extends PlaySpecification {
  "be usable via SecureSocial" in new WithApplication {
    //val request:RequestHeader = new RequestHeaders()
    //  .withCookies(Cookie(Authenticator.cookieName, "somevalue"))
    //RequestHandler.authenticatorFromRequest(request)
    true mustEqual false
  }
}
