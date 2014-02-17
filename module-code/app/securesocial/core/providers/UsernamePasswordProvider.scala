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
package securesocial.core.providers

import play.api.data.Form
import play.api.data.Forms._
import securesocial.core._
import play.api.mvc._
import utils.{GravatarHelper, PasswordHasher}
import play.api.{Play, Application}
import Play.current
import com.typesafe.plugin._
import securesocial.controllers.TemplatesPlugin
import org.joda.time.DateTime
import securesocial.core.IdentityId
import scala.Some
import play.api.mvc.SimpleResult

/**
 * A username password provider
 */
class UsernamePasswordProvider(application: Application) extends IdentityProvider(application) {

  override def id = UsernamePasswordProvider.UsernamePassword

  def authMethod = AuthenticationMethod.UserPassword

  val InvalidCredentials = "securesocial.login.invalidCredentials"

  def doAuth()(implicit request: RequestWithIdentity[AnyContent]): Either[SimpleResult, FlowState] = {
    val form = UsernamePasswordProvider.loginForm.bindFromRequest()
    form.fold(
      errors => Left(badRequest(errors)(request)),
      credentials => {
        val userId = IdentityId(credentials._1, id)
        val result = for (
          user <- UserService.find(userId) ;
          pinfo <- user.passwordInfo ;
          hasher <- Registry.hashers.get(pinfo.hasher) if hasher.matches(pinfo, credentials._2)
        ) yield Right(FlowState(newIdentity=Some(SocialUser(user))))
        result.getOrElse(
          Left(badRequest(UsernamePasswordProvider.loginForm, Some(InvalidCredentials)))
        )
      }
    )
  }

  private def badRequest[A](f: Form[(String,String)], msg: Option[String] = None)(implicit request: Request[AnyContent]): SimpleResult = {
    Results.BadRequest(use[TemplatesPlugin].getLoginPage(f, msg))
  }

  def fillProfile(user: SocialUser) = {
    GravatarHelper.avatarFor(user.email.get) match {
      case Some(url) if url != user.avatarUrl => user.copy( avatarUrl = Some(url))
      case _ => user
    }
  }
}

object UsernamePasswordProvider {
  val UsernamePassword = "userpass"
  private val Key = "securesocial.userpass.withUserNameSupport"
  private val SendWelcomeEmailKey = "securesocial.userpass.sendWelcomeEmail"
  private val EnableGravatarKey = "securesocial.userpass.enableGravatarSupport"
  private val Hasher = "securesocial.userpass.hasher"
  private val EnableTokenJob = "securesocial.userpass.enableTokenJob"
  private val SignupSkipLogin = "securesocial.userpass.signupSkipLogin"

  val loginForm = Form(
    tuple(
      "username" -> nonEmptyText,
      "password" -> nonEmptyText
    )
  )

  lazy val withUserNameSupport = current.configuration.getBoolean(Key).getOrElse(false)
  lazy val sendWelcomeEmail = current.configuration.getBoolean(SendWelcomeEmailKey).getOrElse(true)
  lazy val enableGravatar = current.configuration.getBoolean(EnableGravatarKey).getOrElse(true)
  lazy val hasher = current.configuration.getString(Hasher).getOrElse(PasswordHasher.BCryptHasher)
  lazy val enableTokenJob = current.configuration.getBoolean(EnableTokenJob).getOrElse(true)
  lazy val signupSkipLogin = current.configuration.getBoolean(SignupSkipLogin).getOrElse(false)
}

/**
  * A token used for reset password and sign up operations
 *
  * @param uuid the token id
  * @param email the user email
  * @param creationTime the creation time
  * @param expirationTime the expiration time
  * @param isSignUp a boolean indicating wether the token was created for a sign up action or not
  */
case class Token(uuid: String, email: String, creationTime: DateTime, expirationTime: DateTime, isSignUp: Boolean) {
  def isExpired = expirationTime.isBeforeNow
}
