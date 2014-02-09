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

import securesocial.core._
import play.api.mvc.{SimpleResult, AnyContent, Controller}
import com.typesafe.plugin._
import play.api.Play
import Play.current
import play.api.data.Form
import play.api.data.Forms._
import securesocial.core.providers.utils.{Mailer, RoutesHelper, PasswordValidator}
import play.api.i18n.Messages
import securesocial.core.SecuredRequest
import scala.Some

/**
 * A controller to provide password change functionality
 */
case class ChangeInfo(currentPassword: String, newPassword: String)

trait PasswordChange extends SecureSocialController {
  val CurrentPassword = "currentPassword"
  val InvalidPasswordMessage = "securesocial.passwordChange.invalidPassword"
  val NewPassword = "newPassword"
  val Password1 = "password1"
  val Password2 = "password2"
  val Success = "success"
  val OkMessage = "securesocial.passwordChange.ok"

  /**
   * The property that specifies the page the user is redirected to after changing the password.
   */
  val onPasswordChangeGoTo = "securesocial.onPasswordChangeGoTo"

  /** The redirect target of the handlePasswordChange action. */
  def onHandlePasswordChangeGoTo = Play.current.configuration.getString(onPasswordChangeGoTo).getOrElse(
    RoutesHelper.changePasswordPage().url
  )



  def checkCurrentPassword[A](currentPassword: String)(implicit request: SecuredRequest[A]):Boolean = {
    val maybeHasher = request.identity.passwordInfo.flatMap(p => Registry.hashers.get(p.hasher))
    maybeHasher.map(_.matches(request.identity.passwordInfo.get, currentPassword)).getOrElse(false)
  }

  private def execute[A](f: (SecuredRequest[A], Form[ChangeInfo]) => SimpleResult)(implicit request: SecuredRequest[A]): SimpleResult = {
    val form = Form[ChangeInfo](
      mapping(
        CurrentPassword -> nonEmptyText.verifying(
          Messages(InvalidPasswordMessage), checkCurrentPassword(_)),
        NewPassword ->
          tuple(
            Password1 -> nonEmptyText.verifying( PasswordValidator.constraint ),
            Password2 -> nonEmptyText
          ).verifying(Messages(Registration.PasswordsDoNotMatch), passwords => passwords._1 == passwords._2)

      )((currentPassword, newPassword) => ChangeInfo(currentPassword, newPassword._1))
        ((changeInfo: ChangeInfo) => Some("", ("", "")))
    )

    if ( request.identity.authMethod != AuthenticationMethod.UserPassword) {
      Forbidden
    } else {
      f(request, form)
    }
  }

  def page = SecuredAction { implicit request =>
    execute { (request: SecuredRequest[AnyContent], form: Form[ChangeInfo]) =>
      Ok(use[TemplatesPlugin].getPasswordChangePage(form)(request))
    }
  }

  def handlePasswordChange = SecuredAction { implicit request =>
    execute { (request: SecuredRequest[AnyContent], form: Form[ChangeInfo]) =>
      form.bindFromRequest()(request).fold (
        errors => BadRequest(use[TemplatesPlugin].getPasswordChangePage(errors)(request)),
        info =>  {
          import scala.language.reflectiveCalls
          val newPasswordInfo = Registry.hashers.currentHasher.hash(info.newPassword)
          val u = UserService.save( SocialUser(request.identity).copy( passwordInfo = Some(newPasswordInfo)) )
          implicit val userLang = lang(request)
          Mailer.sendPasswordChangedNotice(u)(request, userLang)
          val result = Redirect(onHandlePasswordChangeGoTo).flashing(Success -> Messages(OkMessage)(lang(request)))
          Events.fire(new PasswordChangeEvent(u))(request).map( result.withSession(_)).getOrElse(result)
        }
      )
    }
  }
}

object PasswordChange extends PasswordChange