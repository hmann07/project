package com.neurocoevo.innovation

import akka.actor.{ActorRef, ActorSystem ,Actor, ActorLogging, Props}

object Innovation {
	case class NewConnectionProposal(fromNeuron: Int, toNeuron: Int)
	case class TrackerEntry(fromNeuron: Int, toNeuron: Int, id: Int)

	case class Tracker(currentNeuronInnovationId: Int = 0,
			currentConnectionInnovationId: Int = 0,
			entries: List[TrackerEntry] = List.empty
			// Map[fromNeuron, Map[toNeuron, Innovation Id]]
			//entries: Map[Int, Map[Int, Int]]
			)
	case class ConnectionConfirmation(c: TrackerEntry)
}

class Innovation extends Actor with ActorLogging {

	import Innovation._

	def receive = innovationTracker(Tracker())


	def innovationTracker(t: Tracker) : Receive = {

		case NewConnectionProposal(n1,n2) =>

			// Key here is that if the connection exists it may be be used in the requesting network but perhaps with a different weight.
			val entry = t.entries.find(te => te.fromNeuron == n1 && te.toNeuron == n2)

			if(entry.get != None){ 
				sender() ! ConnectionConfirmation(entry.get)
			} else {
				sender() ! ConnectionConfirmation(
					new TrackerEntry(n1, n2, t.currentConnectionInnovationId)
					)
				context become innovationTracker(t.copy(currentConnectionInnovationId = t.currentConnectionInnovationId + 1))
			}
			
			//val incr = if (t.currentConnectionInnovationId == confirmation.id) t.currentConnectionInnovationId else 0 
				
			
	}
}
