package com.neurocoevo.innovation

import com.neurocoevo.genome.NetworkGenome

import akka.actor.{ActorRef, ActorSystem ,Actor, ActorLogging, Props}

object Innovation {
	
	// We can assume that all agents being tracked here are members of the same population and therefore also 
	// share the same seed genome. TODO: What happens if we were restarting an experiment? If there were, for whatever reason,
	// multiple genomes we would have to pick max from the multiples.

	def props(seedGenome: NetworkGenome): Props = Props(new Innovation(seedGenome))

	case class NewConnectionProposal(fromNeuron: Int, toNeuron: Int)

	case class NewNeuronProposal(fromNeuron: Int, toNeuron: Int)

	case class ConnectionTrackerEntry(fromNeuron: Int, toNeuron: Int, innovationId: Int)

	case class NeuronTrackerEntry(
		fromNeuron: Int, 
		connection1: Int, 
		newNeuron: Int, 
		connection2: Int,
		toNeuron: Int)

	case class Tracker(
		currentNeuronInnovationId: Int = 0,
		currentConnectionInnovationId: Int = 0,
		neuronEntries: List[NeuronTrackerEntry] = List.empty,
		connectionEntries: List[ConnectionTrackerEntry] = List.empty)

	case class NewConnectionConfirmation(connectionData: ConnectionTrackerEntry)

	case class NewNeuronConfirmation(neuronData: NeuronTrackerEntry)
}

	// Key Ideas. Nodes do not need an innovation number as such but will need an ID.
	// All mutations are essentially based on the connections. 


class Innovation(seedGenome: NetworkGenome) extends Actor with ActorLogging {

	import Innovation._

	val latestConnection = seedGenome.connections.keys.max + 1
	val latestNeuron = seedGenome.neurons.keys.max + 1



	println(latestConnection)
	println(latestNeuron)
	
	def receive = innovationTracker(Tracker(
		currentNeuronInnovationId = latestNeuron, 
		currentConnectionInnovationId = latestConnection))


	def innovationTracker(t: Tracker) : Receive = {

		case NewConnectionProposal(n1,n2) =>

			// need to check if these two nodes, already have a connection between 
			// them.

			// search for a connection that links n1 to n2 NOTE. This is directed.
			val existingEntry = t.connectionEntries.find(te => te.fromNeuron == n1 && te.toNeuron == n2)
			
			existingEntry match {

				case Some(e) => {
			
					// If there is an existing link between n1 and n2 then sent back the data. 
					// The exisitng innovation id will be used 
					
					sender() ! NewConnectionConfirmation(e)
				}

				case None => {

				// There is not an exisitng connection between n1 and n2.
				// create a new tracker entry

				val newEntry =  new ConnectionTrackerEntry(n1, n2, t.currentConnectionInnovationId)

				// send it to the agent

				sender() ! NewConnectionConfirmation(newEntry)

				// store it for future lookups whilst incrementing the counter.

				context become innovationTracker(t.copy(
						currentConnectionInnovationId = t.currentConnectionInnovationId + 1,
						connectionEntries = newEntry :: t.connectionEntries))
			
				}
			}


		case NewNeuronProposal(n1, n2) =>

			// need to check if n1 and n2 have been split before. 
			// if they have not then we split them, and create a new neuron. which will
			// get a new id.
			
			val existingEntry = t.neuronEntries.find(te => te.fromNeuron == n1 && te.toNeuron == n2)

			existingEntry match { 
				
				case Some(e) => {
				// we have previously split this connection. 
				// The exisitng innovation ids will be used 
	
					sender() ! NewNeuronConfirmation(e)
				}
			
				case None => {

					// we have not previously added a neuron between n1 and n2
					// create a new tracker entry with two new connections and one neuron.

					val newEntry =  new NeuronTrackerEntry(
						n1, 
						t.currentConnectionInnovationId, 
						t.currentNeuronInnovationId, 
						t.currentConnectionInnovationId + 1, 
						n2)

					println(newEntry.toString)

					// send it to the agent

					sender() ! NewNeuronConfirmation(newEntry)

					// store it for future lookups whilst incrementing the counter.

					context become innovationTracker(t.copy(
							currentConnectionInnovationId = t.currentConnectionInnovationId + 2,
							currentNeuronInnovationId = t.currentNeuronInnovationId + 1,
							neuronEntries =  newEntry :: t.neuronEntries))
				}
			}
		}		
	}

