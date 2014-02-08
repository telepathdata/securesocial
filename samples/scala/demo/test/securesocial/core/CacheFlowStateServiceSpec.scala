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
      val flowStateId = service.newFlowState(sessionId)
      val valid = service.validateFlowState(flowStateId, sessionId)
      valid mustEqual true
    }
    "be able to store and validate a flowStateId without a sessionId" in new WithApplication {
      val flowStateId = service.newFlowState(None)
      val valid = service.validateFlowState(flowStateId, None)
      valid mustEqual true
    }
    "be able to store and invalidate a flowStateId with a missing new sessionId" in new WithApplication {
      val flowStateId = service.newFlowState(Some("badSession"))
      val valid = service.validateFlowState(flowStateId, None)
      valid mustEqual false
    }
    "be able to store and validate a flowStateId with a bad new sessionId" in new WithApplication {
      val flowStateId = service.newFlowState(None)
      val valid = service.validateFlowState(flowStateId, Some("badSession"))
      valid mustEqual false
    }
  }
}
