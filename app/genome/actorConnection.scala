package com.neurocoevo.genome

import com.neurocoevo.substrate._
import scala.util.Random

import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}

/*
		Connection class holds source and destination connections. along with a randomised weight

*/

class ActorConnection(val innovationId: Int, val source: ActorRef, val destination: ActorRef, val weight: Double, val recurrent: Boolean) {

}
