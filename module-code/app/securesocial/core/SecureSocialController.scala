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
package securesocial.core

import play.api.mvc._
import providers.utils.RoutesHelper
import play.api.i18n.Messages
import play.api.Logger
import play.api.libs.json.Json
import scala.concurrent.Future
import scala.Some
import play.api.mvc.SimpleResult

/**
 * A request that adds the User for the current call
 */
case class SecuredRequest[A](user: Identity, request: Request[A]) extends WrappedRequest(request)

/**
 * A request that adds the User for the current call
 */
case class RequestWithUser[A](user: Option[Identity], request: Request[A]) extends WrappedRequest(request)


/**
 * Provides the actions that can be used to protect controllers and retrieve the current user
 * if available.
 *
 * object MyController extends SecureSocial {
 *    def protectedAction = SecuredAction { implicit request =>
 *      Ok("Hello %s".format(request.user.displayName))
 *    }
 */
trait SecureSocialController extends Controller {
  def authService:AuthenticatorService = AuthenticatorService
  def requestService:RequestService = RequestService
  def userService = UserService

  /**
   * A Forbidden response for ajax clients
   * @param request
   * @tparam A
   * @return
   */
  private def ajaxCallNotAuthenticated[A](implicit request: Request[A]): SimpleResult = {
    Unauthorized(Json.toJson(Map("error"->"Credentials required"))).as(JSON)
  }

  private def ajaxCallNotAuthorized[A](implicit request: Request[A]): SimpleResult = {
    Forbidden( Json.toJson(Map("error" -> "Not authorized"))).as(JSON)
  }

  /**
   * A secured action.  If there is no user in the session the request is redirected
   * to the login page
   */
  object SecuredAction extends SecuredActionBuilder[SecuredRequest[_]] {
    /**
     * Creates a secured action
     */
    def apply[A]() = new SecuredActionBuilder[A](false, None)

    /**
     * Creates a secured action
     *
     * @param ajaxCall a boolean indicating whether this is an ajax call or not
     */
    def apply[A](ajaxCall: Boolean) = new SecuredActionBuilder[A](ajaxCall, None)

    /**
     * Creates a secured action
     * @param authorize an Authorize object that checks if the user is authorized to invoke the action
     */
    def apply[A](authorize: Authorization) = new SecuredActionBuilder[A](false, Some(authorize))

    /**
     * Creates a secured action
     * @param ajaxCall a boolean indicating whether this is an ajax call or not
     * @param authorize an Authorize object that checks if the user is authorized to invoke the action
     */
    def apply[A](ajaxCall: Boolean, authorize: Authorization) = new SecuredActionBuilder[A](ajaxCall, Some(authorize))
  }

/**
   * A builder for secured actions
   *
   * @param ajaxCall a boolean indicating whether this is an ajax call or not
   * @param authorize an Authorize object that checks if the user is authorized to invoke the action
   * @tparam A
   */
  class SecuredActionBuilder[A](ajaxCall: Boolean = false, authorize: Option[Authorization] = None)
    extends ActionBuilder[({ type R[A] = SecuredRequest[A] })#R] {

    def invokeSecuredBlock[A](ajaxCall: Boolean, authorize: Option[Authorization], request: Request[A],
                              block: SecuredRequest[A] => Future[SimpleResult]): Future[SimpleResult] =
    {
      implicit val req = request
      val result = for (
        authenticator <- requestService.authenticatorFromRequest ;
        user <- userService.find(authenticator.identityId)
      ) yield {
        touch(authenticator)
        if ( authorize.isEmpty || authorize.get.isAuthorized(user)) {
          block(SecuredRequest(user, request))
        } else {
          Future.successful {
            if ( ajaxCall ) {
              ajaxCallNotAuthorized(request)
            } else {
              Redirect(RoutesHelper.notAuthorized.absoluteURL(IdentityProvider.sslEnabled))
            }
          }
        }
      }

      result.getOrElse({
        if ( Logger.isDebugEnabled ) {
          Logger.debug("[securesocial] anonymous user trying to access : '%s'".format(request.uri))
        }
        val response = if ( ajaxCall ) {
          ajaxCallNotAuthenticated(request)
        } else {
          Redirect(RoutesHelper.login().absoluteURL(IdentityProvider.sslEnabled))
            .flashing("error" -> Messages("securesocial.loginRequired"))
            .withSession(session + (requestService.OriginalUrlKey -> request.uri)
          )
        }
        Future.successful(response.discardingCookies(authService.discardingCookie))
      })
    }

    def invokeBlock[A](request: Request[A], block: SecuredRequest[A] => Future[SimpleResult]) =
       invokeSecuredBlock(ajaxCall, authorize, request, block)
  }


  /**
   * An action that adds the current user in the request if it's available.
   */
  object UserAwareAction extends ActionBuilder[RequestWithUser] {
    protected def invokeBlock[A](request: Request[A],
                                 block: (RequestWithUser[A]) => Future[SimpleResult]): Future[SimpleResult] =
    {
      implicit val req = request
      val user = for (
        authenticator <- requestService.authenticatorFromRequest ;
        user <- userService.find(authenticator.identityId)
      ) yield {
        touch(authenticator)
        user
      }
      block(RequestWithUser(user, request))
    }
  }

  def touch(authenticator: Authenticator) {
    AuthenticatorService.save(authenticator.touch)
  }
}




