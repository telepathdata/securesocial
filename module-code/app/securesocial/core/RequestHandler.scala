package securesocial.core

import play.api.mvc.RequestHeader

/**
 * Created by erik on 2/4/14.
 */
trait RequestHandler {
  def authenticatorFromRequest(implicit request: RequestHeader): Option[Authenticator]

  /**
   * Get the current logged in user.  This method can be used from public actions that need to
   * access the current user if there's any
   *
   * @param request
   * @tparam A
   * @return
   */
  def currentUser[A](implicit request: RequestHeader):Option[Identity]
}

trait CookieRequestHandler {
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

  def currentUser[A](implicit request: RequestHeader):Option[Identity] = {
    request match {
      case securedRequest: SecuredRequest[_] => Some(securedRequest.user)
      case userAware: RequestWithUser[_] => userAware.user
      case _ => for (
            authenticator <- authenticatorFromRequest ;
            user <- UserService.find(authenticator.identityId)
          ) yield {
            user
          }
    }
  }
}

object RequestHandler extends CookieRequestHandler
