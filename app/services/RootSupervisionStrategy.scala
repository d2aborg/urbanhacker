package services

import akka.actor.SupervisorStrategy.Restart
import akka.actor.{OneForOneStrategy, SupervisorStrategy, SupervisorStrategyConfigurator}

import scala.concurrent.duration._

class RootSupervisionStrategy extends SupervisorStrategyConfigurator {
  override def create(): SupervisorStrategy = OneForOneStrategy(maxNrOfRetries = 100, withinTimeRange = 1 minute) {
    case _: Throwable => Restart
  }
}
