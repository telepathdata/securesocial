package securesocial.core

import _root_.java.util.UUID

/**
 * Created by erik on 2/3/14.
 */
trait FlowStateService {
  /**
   * Store flowStateId, optionally binding it to sessionId
   *
   * @param flowStateId
   * @param sessionId
   */
  def storeFlowState(flowStateId:String, sessionId:Option[String]) {
    // NO-OP by default
  }

  /**
   * Ensure flowStateId is valid in sessionId.
   *
   * @param sessionId
   * @param flowStateId
   * @return
   */
  def validateFlowState(flowStateId:String, sessionId:Option[String]):Boolean = {
    true
  }

  /**
   * Get (and store) a new flowStateId
   *
   * @param sessionId
   * @return
   */
  def newFlowState(sessionId:Option[String]):String = {
    val flowStateId = UUID.randomUUID().toString
    this.storeFlowState(flowStateId, sessionId)
    flowStateId
  }
}


