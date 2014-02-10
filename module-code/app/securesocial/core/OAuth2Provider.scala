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

import _root_.java.net.URLEncoder
import _root_.java.util.UUID
import play.api.{Logger, Play, Application}
import play.api.cache.Cache
import Play.current
import play.api.mvc._
import providers.utils.RoutesHelper
import play.api.libs.ws.WS
import scala.collection.JavaConversions._
import play.api.libs.ws.Response
import scala.Some

/**
 * Base class for all OAuth2 providers
 */
abstract class OAuth2Provider(application: Application, jsonResponse: Boolean = true) extends IdentityProvider(application) {
  val flowStateService:FlowStateService = CacheFlowStateService
  val settings = createSettings()

  def authMethod = AuthenticationMethod.OAuth2

  private def createSettings(): OAuth2Settings = {
    val result = for {
      authorizationUrl <- loadProperty(OAuth2Settings.AuthorizationUrl) ;
      accessToken <- loadProperty(OAuth2Settings.AccessTokenUrl) ;
      clientId <- loadProperty(OAuth2Settings.ClientId) ;
      clientSecret <- loadProperty(OAuth2Settings.ClientSecret)
    } yield {
      val scope = application.configuration.getString(propertyKey + OAuth2Settings.Scope)
      val authorizationUrlParams: Map[String, String] =
        application.configuration.getObject(propertyKey + OAuth2Settings.AuthorizationUrlParams).map{ o =>
          o.unwrapped.toMap.mapValues(_.toString)
        }.getOrElse(Map())
      val accessTokenUrlParams: Map[String, String] =
        application.configuration.getObject(propertyKey + OAuth2Settings.AccessTokenUrlParams).map{ o =>
          o.unwrapped.toMap.mapValues(_.toString)
        }.getOrElse(Map())
      OAuth2Settings(authorizationUrl, accessToken, clientId, clientSecret, scope, authorizationUrlParams, accessTokenUrlParams)
    }
    if ( !result.isDefined ) {
      throwMissingPropertiesException()
    }
    result.get
  }

  private def getAccessToken[A](code: String)(implicit request: Request[A]):OAuth2Info = {
    val params = Map(
      OAuth2Constants.ClientId -> Seq(settings.clientId),
      OAuth2Constants.ClientSecret -> Seq(settings.clientSecret),
      OAuth2Constants.GrantType -> Seq(OAuth2Constants.AuthorizationCode),
      OAuth2Constants.Code -> Seq(code),
      OAuth2Constants.RedirectUri -> Seq(getProviderUri(request))
    ) ++ settings.accessTokenUrlParams.mapValues(Seq(_))
    val call = WS.url(settings.accessTokenUrl).post(params)
    try {
      buildInfo(awaitResult(call))
    } catch {
      case e: Exception => {
        Logger.error("[securesocial] error trying to get an access token for provider %s".format(id), e)
        throw new AuthenticationException()
      }
    }
  }

  protected def buildInfo(response: Response): OAuth2Info = {
      val json = response.json
      if ( Logger.isDebugEnabled ) {
        Logger.debug("[securesocial] got json back [" + json + "]")
      }
      OAuth2Info(
        (json \ OAuth2Constants.AccessToken).as[String],
        (json \ OAuth2Constants.TokenType).asOpt[String],
        (json \ OAuth2Constants.ExpiresIn).asOpt[Int],
        (json \ OAuth2Constants.RefreshToken).asOpt[String]
      )
  }

  /**
   * Get the callback URI for this provider. Replaces RoutesHelper but handles inherited
   * ProviderControllers properly.
   *
   * Replaces: RoutesHelper.authenticate(id).absoluteURL(IdentityProvider.sslEnabled)
   *
   * @param request
   * @return
   */
  def getProviderUri(implicit request: RequestHeader):String = {
    Call("GET", request.path).absoluteURL(IdentityProvider.sslEnabled)
  }

  def doAuth()(implicit request: Request[AnyContent]): Either[SimpleResult, SocialUser] = {
    request.queryString.get(OAuth2Constants.Error).flatMap(_.headOption).map( error => {
      error match {
        case OAuth2Constants.AccessDenied => throw new AccessDeniedException()
        case _ =>
          Logger.error("[securesocial] error '%s' returned by the authorization server. Provider type is %s".format(error, id))
          throw new AuthenticationException()
      }
      throw new AuthenticationException()
    })

    request.queryString.get(OAuth2Constants.Code).flatMap(_.headOption) match {
      case Some(code) => completeOAuthFlow(request, code)
      case None => initiateOAuthFlow(request)
    }
  }


  def completeOAuthFlow(implicit request: Request[AnyContent], code: String): Right[Nothing, SocialUser] = {
    // we're being redirected back from the authorization server with the access code.
    val sessionId = request.session.get(IdentityProvider.SessionId)
    val flowStateId = request.queryString.get(OAuth2Constants.State).flatMap(_.headOption)
    val identity = if (flowStateId.isDefined && flowStateService.validateFlowState(flowStateId.get, sessionId)) {
      val accessToken = getAccessToken(code)
      val oauth2Info = Some(
        OAuth2Info(accessToken.accessToken, accessToken.tokenType, accessToken.expiresIn, accessToken.refreshToken)
      )
      Some(SocialUser(IdentityId("", id), "", "", "", None, None, authMethod, oAuth2Info = oauth2Info))
    } else {
      None
    }
    if (Logger.isDebugEnabled) {
      Logger.debug("[securesocial] user = " + identity)
    }
    identity match {
      case Some(u) => Right(u)
      case _ => throw new AuthenticationException()
    }
  }

  def initiateOAuthFlow(implicit request: Request[AnyContent]): Left[SimpleResult, Nothing] = {
    // There's no code in the request, this is the first step in the oauth flow
    val sessionId = request.session.get(IdentityProvider.SessionId).getOrElse(UUID.randomUUID().toString)
    val state = flowStateService.newFlowState(Some(sessionId))
    var params = List(
      (OAuth2Constants.ClientId, settings.clientId),
      (OAuth2Constants.RedirectUri, getProviderUri(request)),
      (OAuth2Constants.ResponseType, OAuth2Constants.Code),
      (OAuth2Constants.State, state))
    settings.scope.foreach(s => {
      params = (OAuth2Constants.Scope, s) :: params
    })
    settings.authorizationUrlParams.foreach(e => {
      params = e :: params
    })
    val url = settings.authorizationUrl +
      params.map(p => URLEncoder.encode(p._1, "UTF-8") + "=" + URLEncoder.encode(p._2, "UTF-8")).mkString("?", "&", "")
    if (Logger.isDebugEnabled) {
      Logger.debug("[securesocial] authorizationUrl = %s".format(settings.authorizationUrl))
      Logger.debug("[securesocial] redirecting to: [%s]".format(url))
    }
    Left(Results.Redirect(url).withSession(request.session +(IdentityProvider.SessionId, sessionId)))
  }
}

case class OAuth2Settings(authorizationUrl: String, accessTokenUrl: String, clientId: String,
                          clientSecret: String, scope: Option[String],
                          authorizationUrlParams: Map[String, String], accessTokenUrlParams: Map[String, String]
                           )

object OAuth2Settings {
  val AuthorizationUrl = "authorizationUrl"
  val AccessTokenUrl = "accessTokenUrl"
  val AuthorizationUrlParams = "authorizationUrlParams"
  val AccessTokenUrlParams = "accessTokenUrlParams"
  val ClientId = "clientId"
  val ClientSecret = "clientSecret"
  val Scope = "scope"
}

object OAuth2Constants {
  val ClientId = "client_id"
  val ClientSecret = "client_secret"
  val RedirectUri = "redirect_uri"
  val Scope = "scope"
  val ResponseType = "response_type"
  val State = "state"
  val GrantType = "grant_type"
  val AuthorizationCode = "authorization_code"
  val AccessToken = "access_token"
  val Error = "error"
  val Code = "code"
  val TokenType = "token_type"
  val ExpiresIn = "expires_in"
  val RefreshToken = "refresh_token"
  val AccessDenied = "access_denied"
}
