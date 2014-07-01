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
import play.api.Application
import play.api.mvc._
import play.api.libs.ws.WS
import scala.collection.JavaConversions._
import play.api.libs.ws.WSResponse
import play.api.Play.current
import com.typesafe.scalalogging.slf4j.LazyLogging
import play.api.libs.json.Json
import org.joda.time.DateTime

/**
 * Base class for all OAuth2 providers
 */
abstract class OAuth2Provider(application: Application, jsonResponse: Boolean = true)
  extends IdentityProvider(application)
  with LazyLogging
{
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
        logger.error("error trying to get an access token for provider %s".format(id), e)
        throw new AuthenticationException()
      }
    }
  }

  def refresh(info: OAuth2Info): OAuth2Info = {
    logger.debug("Refreshing " + info)
    val newInfo = for {
      refreshToken <- info.refreshToken
    } yield {
      val params = Map(
        OAuth2Constants.ClientId -> Seq(settings.clientId),
        OAuth2Constants.ClientSecret -> Seq(settings.clientSecret),
        OAuth2Constants.GrantType -> Seq(OAuth2Constants.RefreshToken),
        OAuth2Constants.RefreshToken -> Seq(refreshToken)
      )
      val call = WS.url(settings.accessTokenUrl).post(params)
      val json = awaitResult(call).json
      logger.debug("Got json" + json)
      info.copy(
        accessToken = (json \ OAuth2Constants.AccessToken).as[String],
        tokenType = (json \ OAuth2Constants.TokenType).asOpt[String],
        expiresIn = (json \ OAuth2Constants.ExpiresIn).asOpt[Int],
        granted = Some(DateTime.now())
      )
    }
    newInfo.getOrElse(info)
  }

  protected def buildInfo(response: WSResponse): OAuth2Info = {
      val json = response.json
      logger.debug("got json back [" + json + "]")
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
  def getProviderUri(implicit request: RequestHeader, ajaxMode:Boolean=false):String = {
    Call("GET", request.path).absoluteURL(IdentityProvider.sslEnabled)
  }

  def doAuth()(implicit request: RequestWithIdentity[AnyContent]): Either[Result, FlowState] = {
    request.queryString.get(OAuth2Constants.Error).flatMap(_.headOption).map( error => {
      error match {
        case OAuth2Constants.AccessDenied => throw new AccessDeniedException()
        case _ =>
          logger.error("error '%s' returned by the authorization server. Provider type is %s".format(error, id))
          throw new AuthenticationException()
      }
      throw new AuthenticationException()
    })

    request.queryString.get(OAuth2Constants.Code).flatMap(_.headOption) match {
      case Some(code) => completeOAuthFlow(request, code)
      case None => initiateOAuthFlow(request)
    }
  }

  def initiateOAuthFlow(implicit request: RequestWithIdentity[AnyContent]): Left[Result, Nothing] = {
    // There's no code in the request, this is the first step in the oauth flow
    val sessionId = request.session.get(IdentityProvider.SessionId).getOrElse(UUID.randomUUID().toString)
    val ajaxMode = request.getQueryString("mode").getOrElse("redirect") == "ajax"
    val email = request.getQueryString("email").getOrElse("")
    val flowState = flowStateService.newFlowState(Some(sessionId), request.identity, ajaxMode)
    logger.debug("initiating OAuth flow: " + flowState)
    var params = List(
      (OAuth2Constants.ClientId, settings.clientId),
      (OAuth2Constants.RedirectUri, getProviderUri(request, ajaxMode)),
      (OAuth2Constants.ResponseType, OAuth2Constants.Code),
      (OAuth2Constants.AccessType, OAuth2Constants.Offline),
      (OAuth2Constants.State, flowState.id),
      (OAuth2Constants.LoginHint,email))
    settings.scope.foreach(s => {
      params = (OAuth2Constants.Scope, s) :: params
    })
    settings.authorizationUrlParams.foreach(e => {
      params = e :: params
    })
    val url = settings.authorizationUrl +
      params.map(p => URLEncoder.encode(p._1, "UTF-8") + "=" + URLEncoder.encode(p._2, "UTF-8")).mkString("?", "&", "")
    logger.debug("authorizationUrl = %s".format(settings.authorizationUrl))
    logger.debug("redirecting to: [%s]".format(url))
    if (ajaxMode) {
      Left(Results.Ok(Json.obj("state" -> flowState.id, "url" -> url)))
    } else {
      Left(Results.Redirect(url).withSession(request.session +(IdentityProvider.SessionId, sessionId)))
    }
  }

  def completeOAuthFlow(implicit request: Request[AnyContent], code: String): Right[Nothing, FlowState] = {
    // we're being redirected back from the authorization server with the access code.
    val flowStateId = request.queryString.get(OAuth2Constants.State).flatMap(_.headOption)
    var flowState = flowStateService.get(flowStateId.get).getOrElse(throw new AuthenticationException())
    logger.debug("completing OAuth flow: " + flowState)
    val oauth2Info = Some(getAccessToken(code))
    flowState = flowState.copy(newIdentity =
      Some(SocialUser(IdentityId("", id), "", "", "", None, None, authMethod, oAuth2Info = oauth2Info))
    )
    Right(flowState)
  }
}

case class OAuth2Settings(authorizationUrl: String,
                          accessTokenUrl: String,
                          clientId: String,
                          clientSecret: String,
                          scope: Option[String],
                          authorizationUrlParams: Map[String, String],
                          accessTokenUrlParams: Map[String, String]
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
  val AccessType = "access_type"
  val Online = "online"
  val Offline = "offline"
  val Error = "error"
  val Code = "code"
  val TokenType = "token_type"
  val ExpiresIn = "expires_in"
  val RefreshToken = "refresh_token"
  val IdToken = "id_token"
  val AccessDenied = "access_denied"
  val LoginHint = "login_hint"
}
