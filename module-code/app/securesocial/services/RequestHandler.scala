package securesocial.services

import play.api.mvc.RequestHeader
import securesocial.core._
import securesocial.core.SecuredRequest
import securesocial.core.RequestWithUser

/**
 * Created by erik on 2/4/14.
 */
trait RequestHandler {
  def userService: UserService = UserService

  def authenticatorFromRequest(implicit request: RequestHeader): Option[Authenticator]

  /**
   * Get the current logged in user.  This method can be used from public actions that need to
   * access the current user if there's any
   *
   * @param request
   * @tparam A
   * @return
   */
  def currentUser[A](implicit request: RequestHeader):Option[Identity] = {
    request match {
      case securedRequest: SecuredRequest[_] => Some(securedRequest.user)
      case userAware: RequestWithUser[_] => userAware.user
      case _ => for (
            authenticator <- authenticatorFromRequest ;
            user <- userService.find(authenticator.identityId)
          ) yield {
            user
          }
    }
  }
}



