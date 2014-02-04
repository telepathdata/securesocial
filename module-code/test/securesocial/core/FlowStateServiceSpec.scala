package securesocial.core

import play.api.test.PlaySpecification

trait FlowStateServiceSpec extends PlaySpecification {
  def service:FlowStateService = null

  "the flowstateservice" should {
    "be able to generate a new flowstate" in {
      service.newFlowState.length() mustEqual 36
    }
  }

}

class CacheFlowStateServiceSpec extends FlowStateServiceSpec {
  override def service = CacheFlowStateService
}
