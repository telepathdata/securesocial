package securesocial.core

import play.api.cache.Cache
import play.api.Play.current

/**
 * Created by erik on 2/3/14.
 */
object CacheFlowStateService extends FlowStateService {
  def lifeTime:Int = 300

  /**
   * Ensure flowStateId is valid in sessionId.
   *
   * @param sessionId
   * @param flowStateId
   * @return
   */
  override def validateFlowState(flowStateId: String, sessionId: Option[String] = None): Boolean = {
    val oldFlowStateWrapper = Cache.get(flowStateId)
    if (oldFlowStateWrapper == None) {
      return false
    }
    val oldFlowState: FlowState = oldFlowStateWrapper.get.asInstanceOf[FlowState]
    oldFlowState.sessionId == sessionId
  }


  override def get(flowStateId: String): Option[FlowState] = {
    Cache.get(flowStateId) map { _.asInstanceOf[FlowState] }
  }

  /**
   * Store flowStateId, optionally binding it to sessionId
   *
   * @param flowState
   */
  override def storeFlowState(flowState: FlowState): Unit = {
    Cache.set(flowState.id, flowState, lifeTime)
  }
}
