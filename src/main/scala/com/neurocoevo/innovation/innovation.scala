package com.neurocoevo.innovation

import akka.actor.{ActorRef, ActorSystem ,Actor, ActorLogging, Props}

object Innovation {
	case class NewConnectionProposal(fromNeuron: Int, toNeuron: Int)
	case class NewNeuronProposal(fromNeuron: Int, toNeuron: Int)

	case class TrackerEntry(fromNeuron: Int, toNeuron: Int, id: Int)

	case class Tracker(currentNeuronInnovationId: Int = 0,
			currentConnectionInnovationId: Int = 0,
			neuronEntries: List[TrackerEntry] = List.empty
			connectionEntries: List[TrackerEntry] = List.empty
			// Map[fromNeuron, Map[toNeuron, Innovation Id]]
			//entries: Map[Int, Map[Int, Int]]
			)
	case class ConnectionConfirmation(c: TrackerEntry)
}

	// Key Ideas. Nodes do not need an innovation number as such but will need an ID.
	// All mutations are essentially based on the connections. 


class Innovation extends Actor with ActorLogging {

	import Innovation._

	def receive = innovationTracker(Tracker())


	def innovationTracker(t: Tracker) : Receive = {

		case NewConnectionProposal(n1,n2) =>

			// need to check if these two nodes, already have a connection between 
			// them.

			// search for a connection that links n1 to n2
			val entry = t.connectionEntries.find(te => te.fromNeuron == n1 && te.toNeuron == n2)
			

			if(entry.get != None){ 
				
				// If there is an existing link between n1 and n2 then sent back the data. 
				// The exisitng innovation id will be used 
				
				sender() ! ConnectionConfirmation(entry.get)
			
			} else {

				// There is not an exisitng connection between n1 and n2.
				// create a new tracker entry

				val newEntry =  new TrackerEntry(n1, n2, t.currentConnectionInnovationId)

				// send it to the agent

				sender() ! ConnectionConfirmation(newEntry)

				// store it for future lookups whilst incrementing the counter.

				context become innovationTracker(t.copy(
						currentConnectionInnovationId = t.currentConnectionInnovationId + 1,
						connectionEntries = t.connectionEntries ++ newEntry))
			
			}


		case NewNeuronProposal(n1, n2) =>

			// need to check if n1 and n2 have been split before. 
			// if they have not then we split them, and create a new neuron. which will
			// get a new id.
			checkerTracker(n1, n2, 0)
			
	}
}
