package securesocial.core

import play.api.Play
import play.api.Play.current
import com.typesafe.plugin._
import play.api.mvc.DiscardingCookie
import org.joda.time.DateTime

trait AuthenticatorService {
  // property keys
  val CookieNameKey = "securesocial.cookie.name"
  val CookiePathKey = "securesocial.cookie.path"
  val CookieDomainKey = "securesocial.cookie.domain"
  val CookieHttpOnlyKey = "securesocial.cookie.httpOnly"
  val ApplicationContext = "application.context"
  val IdleTimeoutKey = "securesocial.cookie.idleTimeoutInMinutes"
  val AbsoluteTimeoutKey = "securesocial.cookie.absoluteTimeoutInMinutes"
  val TransientKey = "securesocial.cookie.makeTransient"

  // default values
  val DefaultCookieName = "id"
  val DefaultCookiePath = "/"
  val DefaultCookieHttpOnly = true
  val Transient = None
  val DefaultIdleTimeout = 30
  val DefaultAbsoluteTimeout = 12 * 60


  lazy val cookieName = Play.application.configuration.getString(CookieNameKey).getOrElse(DefaultCookieName)
  lazy val cookiePath = Play.application.configuration.getString(CookiePathKey).getOrElse(
    Play.configuration.getString(ApplicationContext).getOrElse(DefaultCookiePath)
  )
  lazy val cookieDomain = Play.application.configuration.getString(CookieDomainKey)
  lazy val cookieSecure = IdentityProvider.sslEnabled
  lazy val cookieHttpOnly = Play.application.configuration.getBoolean(CookieHttpOnlyKey).getOrElse(DefaultCookieHttpOnly)
  lazy val idleTimeout = Play.application.configuration.getInt(IdleTimeoutKey).getOrElse(DefaultIdleTimeout)
  lazy val absoluteTimeout = Play.application.configuration.getInt(AbsoluteTimeoutKey).getOrElse(DefaultAbsoluteTimeout)
  lazy val absoluteTimeoutInSeconds = absoluteTimeout * 60
  lazy val makeTransient = Play.application.configuration.getBoolean(TransientKey).getOrElse(true)

  val discardingCookie: DiscardingCookie = {
    DiscardingCookie(cookieName, cookiePath, cookieDomain, cookieSecure)
  }

  /**
   * Creates a new authenticator id for the specified user
   *
   * @param user the user Identity
   * @return an authenticator or error if there was a problem creating it
   */
  def create(user: Identity): Either[Error, Authenticator] = {
    val id = use[IdGenerator].generate
    val now = DateTime.now()
    val expirationDate = now.plusMinutes(absoluteTimeout)
    val authenticator = Authenticator(id, user.identityId, now, now, expirationDate)
    val r = use[AuthenticatorStore].save(authenticator)
    val result = r.fold( e => Left(e), _ => Right(authenticator) )
    result
  }

  /**
   * Saves or updates the authenticator in the store
   *
   * @param authenticator the authenticator
   * @return Error if there was a problem saving the authenticator or Unit if all went ok
   */
  def save(authenticator: Authenticator): Either[Error, Unit] = {
    use[AuthenticatorStore].save(authenticator)
  }
  /**
   * Finds an authenticator by id
   *
   * @param id the authenticator id
   * @return Error if there was a problem finding the authenticator or an optional authenticator if all went ok
   */
  def find(id: String): Either[Error, Option[Authenticator]] = {
    use[AuthenticatorStore].find(id)
  }

  /**
   * Deletes an authenticator
   *
   * @param id the authenticator id
   * @return Error if there was a problem deleting the authenticator or Unit if all went ok
   */
  def delete(id: String): Either[Error, Unit] = {
    use[AuthenticatorStore].delete(id)
  }
}

object AuthenticatorService extends AuthenticatorService
