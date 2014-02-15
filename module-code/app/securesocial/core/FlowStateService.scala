package securesocial.core

import _root_.java.util.UUID

case class FlowState(
  id: String,
  ajaxMode: Boolean,
  sessionId: Option[String],
  mainIdentity: Option[Identity]
)

trait FlowStateService {
  /**
   * Store flowStateId, optionally binding it to sessionId
   *
   * @param flowState
   */
  def storeFlowState(flowState: FlowState) {
    // NO-OP by default
  }

  /**
   * Ensure flowStateId is valid in sessionId.
   *
   * @param flowStateId
   * @param sessionId
   * @return
   */
  def validateFlowState(flowStateId: String, sessionId: Option[String] = None):Boolean = {
    true
  }

  def get(flowStateId: String): Option[FlowState]

  /**
   * Get (and store) a new flowStateId
   *
   * @param sessionId
   * @return
   */
  def newFlowState(
    sessionId:Option[String],
    mainIdentity: Option[Identity],
    ajaxMode:Boolean = false
  ):FlowState = {
    var flowState = FlowState(UUID.randomUUID().toString, ajaxMode, sessionId, mainIdentity)
    this.storeFlowState(flowState)
    flowState
  }
}


