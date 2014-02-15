package securesocial.core

import play.api.test.{WithApplication, PlaySpecification}

/**
 * Created by erik on 2/8/14.
 */
class CacheFlowStateServiceSpec extends PlaySpecification {
  val service = CacheFlowStateService
  "the CacheFlowStateServiceSpec" should {
    "be able to store and validate a flowStateId with a sessionId" in new WithApplication {
      val sessionId = Some("mySession")
      val flowState = service.newFlowState(sessionId, None)
      val valid = service.validateFlowState(flowState.id, sessionId)
      valid mustEqual true
    }
    "be able to store and validate a flowStateId without a sessionId" in new WithApplication {
      val flowState = service.newFlowState(None, None)
      val valid = service.validateFlowState(flowState.id, None)
      valid mustEqual true
    }
    "be able to store and invalidate a flowStateId with a missing new sessionId" in new WithApplication {
      val flowState = service.newFlowState(Some("badSession"), None)
      val valid = service.validateFlowState(flowState.id, None)
      valid mustEqual false
    }
    "be able to store and validate a flowStateId with a bad new sessionId" in new WithApplication {
      val flowState = service.newFlowState(None, None)
      val valid = service.validateFlowState(flowState.id, Some("badSession"))
      valid mustEqual false
    }
    "be able to store and validate a flowStateId with an identity" in new WithApplication {
      val flowState = service.newFlowState(None, Some(SocialUser(
        IdentityId("test", "domain.com"), "test", "user", "test user", Some("test.user@domain.com"),
        None, AuthenticationMethod.UserPassword)
      ))
      val valid = service.validateFlowState(flowState.id, None)
      valid mustEqual true
    }
  }
}
