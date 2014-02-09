package securesocial.core

import play.api.test.PlaySpecification

trait FlowStateServiceSpec extends PlaySpecification {
  def service:FlowStateService = null

  "the flowstateservice" should {
    "be able to generate a new flowstate" in new WithApplication {
      service.newFlowState(None).length() mustEqual 36
    }
  }

}

