package securesocial.services

import play.api.mvc.RequestHeader
import securesocial.core.{Identity, Authenticator}

/**
 * Created by erik on 2/9/14.
 */
trait CookieRequestHandler extends RequestHandler {
  def authenticatorFromRequest(implicit request: RequestHeader): Option[Authenticator] = {
    val result = for {
      cookie <- request.cookies.get(Authenticator.cookieName)
      maybeAuthenticator <- Authenticator.find(cookie.value).fold(e => None, Some(_));
      authenticator <- maybeAuthenticator
    } yield {
      authenticator
    }

    result match {
      case Some(a) => {
        if (!a.isValid) {
          Authenticator.delete(a.id)
          None
        } else {
          Some(a)
        }
      }
      case None => None
    }
  }
}

object CookieRequestHandler extends CookieRequestHandler
