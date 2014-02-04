package securesocial.core

import _root_.java.util.UUID

/**
 * Created by erik on 2/3/14.
 */
trait FlowStateService {
  def newFlowState:String = {
    UUID.randomUUID().toString
  }
}


