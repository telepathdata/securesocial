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
    val oldSessionIdWrapper = Cache.get(flowStateId)
    if (oldSessionIdWrapper == None) {
      return false
    }
    val oldSessionId: Option[String] = oldSessionIdWrapper.get.asInstanceOf[Option[String]]
    oldSessionId == sessionId
  }

  /**
   * Store flowStateId, optionally binding it to sessionId
   *
   * @param flowStateId
   * @param sessionId
   */
  override def storeFlowState(flowStateId: String, sessionId: Option[String]): Unit = {
    Cache.set(flowStateId, sessionId, lifeTime)
  }
}
