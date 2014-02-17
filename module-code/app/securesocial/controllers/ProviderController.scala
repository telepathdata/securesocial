/**
 * Copyright 2012-2014 Jorge Aliss (jaliss at gmail dot com) - twitter: @jaliss
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package securesocial.controllers

import play.api.mvc._
import play.api.i18n.Messages
import securesocial.core._
import play.api.{Play}
import Play.current
import providers.utils.RoutesHelper
import securesocial.core.LoginEvent
import securesocial.core.AccessDeniedException
import scala.Some
import play.api.http.HeaderNames
import securesocial.core.RequestService
import com.typesafe.scalalogging.slf4j.Logging


/**
 * A controller to provide the authentication entry point
 */
class ProviderController extends SecureSocialController with Logging
{
  /**
   * The property that specifies the page the user is redirected to if there is no original URL saved in
   * the session.
   */
  val onLoginGoTo = "securesocial.onLoginGoTo"

  /**
   * The root path
   */
  val Root = "/"

  /**
   * The application context
   */
  val ApplicationContext = "application.context"

  /**
   * Returns the url that the user should be redirected to after login
   *
   * @param session
   * @return
   */
  def toUrl(session: Session) = session.get(requestService.OriginalUrlKey).getOrElse(landingUrl)

  /**
   * The url where the user needs to be redirected after succesful authentication.
   *
   * @return
   */
  def landingUrl = Play.configuration.getString(onLoginGoTo).getOrElse(
    Play.configuration.getString(ApplicationContext).getOrElse(Root)
  )

  /**
   * Renders a not authorized page if the Authorization object passed to the action does not allow
   * execution.
   *
   * @see Authorization
   */
  def notAuthorized() = Action { implicit request =>
    import com.typesafe.plugin._
    Forbidden(use[TemplatesPlugin].getNotAuthorizedPage)
  }

  /**
   * The authentication flow for all providers starts here.
   *
   * @param provider The id of the provider that needs to handle the call
   * @return
   */
  def authenticate(provider: String, redirectTo: Option[String] = None) = handleAuth(provider, redirectTo)
  def authenticateByPost(provider: String, redirectTo: Option[String] = None) = handleAuth(provider, redirectTo)

  private def overrideOriginalUrl(session: Session, redirectTo: Option[String]) = redirectTo match {
    case Some(url) =>
      session + (RequestService.OriginalUrlKey -> url)
    case _ =>
      session
  }

  private def handleAuth(provider: String, redirectTo: Option[String]) = UserAwareAction { implicit request =>
    val modifiedSession = overrideOriginalUrl(session, redirectTo)

    Registry.providers.get(provider) match {
      case Some(p) => {
        try {
          p.authenticate().fold(
            result => handleAuthPhase1(redirectTo, result),
            flowState => handleAuthPhase2(flowState, modifiedSession)
          )
        } catch {
          case ex: AccessDeniedException => {
            Redirect(RoutesHelper.login()).flashing("error" -> Messages("securesocial.login.accessDenied"))
          }
          case other: Throwable => {
            logger.error("Unable to log user in. An exception was thrown", other)
            Redirect(RoutesHelper.login()).flashing("error" -> Messages("securesocial.login.errorLoggingIn"))
          }
        }
      }
      case _ => NotFound
    }
  }


  def handleAuthPhase1(redirectTo: Option[String], result: SimpleResult): SimpleResult = {
    redirectTo match {
      case Some(url) =>
        addSessionCookie(result, url)
      case _ => result
    }
  }

  def handleAuthPhase2(flowState: FlowState, modifiedSession: Session)
                      (implicit request: RequestHeader): SimpleResult = {
    logger.debug("handleAuthPhase2")
    var f: FlowState = flowState
    if (f.mainIdentity.isDefined) {
      f = f.copy(mainIdentity = Some(UserService.save(f.mainIdentity.get)))
    }
    if (f.newIdentity.isDefined) {
      f = f.copy(newIdentity = Some(UserService.save(f.newIdentity.get)))
    }
    if (f.mainIdentity.isDefined && f.newIdentity.isDefined) {
      UserService.link(f.mainIdentity.get, f.newIdentity.get)
      logger.debug(s"linked $f.mainIdentity to $f.newIdentity")
    }
    completeAuthentication(f, modifiedSession)
  }

  def addSessionCookie(result: SimpleResult, url: String): SimpleResult = {
    val cookies = Cookies(result.header.headers.get(HeaderNames.SET_COOKIE))
    val resultSession = Session.decodeFromCookie(cookies.get(Session.COOKIE_NAME))
    result.withSession(resultSession + (RequestService.OriginalUrlKey -> url))
  }

  def completeAuthentication(flowState: FlowState, session: Session)
                            (implicit request: RequestHeader): SimpleResult = {
    logger.debug("user logged in : [" + flowState.newIdentity + "]")
    val withSession = Events.fire(new LoginEvent(flowState.newIdentity.get)).getOrElse(session)
    authService.create(flowState.newIdentity.get) match {
      case Right(authenticator) =>
        Redirect(toUrl(withSession)).withSession(withSession-
          RequestService.OriginalUrlKey -
          IdentityProvider.SessionId -
          OAuth1Provider.CacheKey)
          .withCookies(authenticator.toCookie)
      case Left(error) => {
        // improve this
        throw new RuntimeException("Error creating authenticator")
      }
    }
  }
}

object ProviderController extends ProviderController
