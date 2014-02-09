package securesocial.core

import play.api.mvc.{Request, Result, RequestHeader}
import play.api.libs.oauth.ServiceInfo
import play.api.http.HeaderNames
import securesocial.services.{CookieRequestHandler, RequestHandler}

/**
 * Created by erik on 2/9/14.
 */
object RequestService extends RequestService

trait RequestService {
  def requestHandler:RequestHandler = CookieRequestHandler
  val OriginalUrlKey = "original-url"

  def authenticatorFromRequest(implicit request:RequestHeader) = requestHandler.authenticatorFromRequest
  def currentUser[A](implicit request: RequestHeader) = requestHandler.currentUser


  /**
   * Returns the ServiceInfo needed to sign OAuth1 requests.
   *
   * @param user the user for which the serviceInfo is needed
   * @return an optional service info
   */
  def serviceInfoFor(user: Identity): Option[ServiceInfo] = {
    Registry.providers.get(user.identityId.providerId) match {
      case Some(p: OAuth1Provider) if p.authMethod == AuthenticationMethod.OAuth1 => Some(p.serviceInfo)
      case _ => None
    }
  }

  /**
   * Saves the referer as original url in the session if it's not yet set.
   * @param result the result that maybe enhanced with an updated session
   * @return the result that's returned to the client
   */
  def withRefererAsOriginalUrl[A](result: Result)(implicit request: Request[A]): Result = {
    request.session.get(OriginalUrlKey) match {
      // If there's already an original url recorded we keep it: e.g. if s.o. goes to
      // login, switches to signup and goes back to login we want to keep the first referer
      case Some(_) => result
      case None => {
        request.headers.get(HeaderNames.REFERER).map { referer =>
          // we don't want to use the ful referer, as then we might redirect from https
          // back to http and loose our session. So let's get the path and query string only
          val idxFirstSlash = referer.indexOf("/", "https://".length())
          val refererUri = if (idxFirstSlash < 0) "/" else referer.substring(idxFirstSlash)
          result.withSession(
            request.session + (OriginalUrlKey -> refererUri))
        }.getOrElse(result)
      }
    }
  }

  val enableRefererAsOriginalUrl = {
    import play.api.Play
    Play.current.configuration.getBoolean("securesocial.enableRefererAsOriginalUrl").getOrElse(false)
  }
}