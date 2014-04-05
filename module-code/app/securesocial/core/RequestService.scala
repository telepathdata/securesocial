package securesocial.core

import play.api.mvc.{Request, Result, RequestHeader}
import play.api.libs.oauth.ServiceInfo
import play.api.http.HeaderNames
import org.apache.commons.codec.binary.{StringUtils, Base64}
import securesocial.core.providers.UsernamePasswordProvider

trait RequestService {
  def userService: UserService = UserService
  def authService: AuthenticatorService = AuthenticatorService
  def identityService:UserService = UserService
  val OriginalUrlKey = "original-url"

  /**
   * Extract an authorization token from a request
   *
   * @param request
   * @return
   */
  def tokenFromRequest(implicit request: RequestHeader): Option[String] = {
    request.cookies.get(authService.cookieName) map { _.value }
  }

  /**
   * Extract HTTP Basic parameters from a request
   *
   * @param request
   * @return
   */

  def basicParamsFromRequest(implicit request: RequestHeader): Option[(String, String)] = {
    request.headers.get("Authorization") match {
      case None => None
      case Some(header:String) =>
        if (header.contains("Basic")) {
          val token = header.split("Basic ")(1)
          val tokenStr:String = StringUtils.newStringUtf8(Base64.decodeBase64(token.getBytes))
          val splits = tokenStr.split(":")
          val user = splits(0)
          val pass = splits(1)
          Some((user,pass))
        } else {
          None
        }
    }
  }


  /**
   * Get an authenticator from a given request
   *
   * @param request
   * @return
   */
  def authenticatorFromRequest(implicit request: RequestHeader): Option[Authenticator] = {
    val authenticator = for {
      token <- tokenFromRequest
      maybeAuthenticator <- authService.find(token).fold(e => None, Some(_))
      authenticator <- maybeAuthenticator
    } yield {
      authenticator
    }

    authenticator match {
      case Some(a) => {
        if (!a.isValid) {
          authService.delete(a.id)
          None
        } else {
          Some(a)
        }
      }
      case None => None
    }
  }


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
      case securedRequest: SecuredRequest[_] => Some(securedRequest.identity)
      case userAware: RequestWithIdentity[_] => userAware.identity
      case _ =>
        // do token auth
        for (
          authenticator <- authenticatorFromRequest ;
          user <- userService.find(authenticator.identityId)
        ) yield {
          return Some(user)
        }
        // do basic auth
        basicParamsFromRequest match {
          case None => None
          case Some(t: (String,String)) => login(t._1, t._2)
        }
    }
  }

  /**
   * Touch an authenticator, causing it's expiration date to be moved forward
   * @param authenticator
   */
  def touch(authenticator: Authenticator) {
    authService.save(authenticator.touch)
  }

  /**
   * Retrieve an identity record from a request, if possible.
   *
   * @param request
   * @return
   */
  def identityFromRequest(implicit request: RequestHeader): Option[Identity] = {
    val identity = for (
      authenticator <- authenticatorFromRequest ;
      identity <- identityService.find(authenticator.identityId)
    ) yield {
      touch(authenticator)
      identity
    }

    val identity2 = identity match {
      case Some(identity:Identity) => Some(identity)
      case None =>
        basicParamsFromRequest match {
          case None => None
          case Some(t: (String,String)) => login(t._1, t._2)
        }
    }
    identity2
  }


  /**
   * Get an identity, if possible, from an email and password
   *
   * @param email
   * @param password
   * @return
   */
  def login(email:String, password:String): Option[Identity] = {
    for {
      identity <- UserService.findByEmailAndProvider(email, UsernamePasswordProvider.UsernamePassword)
      pInfo <- identity.passwordInfo
      hasher <- Registry.hashers.get(pInfo.hasher)
      if hasher.matches(pInfo, password)
    } yield {
      identity
    }
  }


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

object RequestService extends RequestService
