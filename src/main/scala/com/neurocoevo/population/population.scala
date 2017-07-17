package com.neurocoevo.population

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}
import com.neurocoevo.agent.Agent
import com.neurocoevo.experience._
import com.neurocoevo.network._
import com.neurocoevo.innovation._
import scala.util.Random
import scala.collection.immutable.HashMap

import com.neurocoevo.genome._

object Population {
	case class PopulationSettings(n: Int, g: NetworkGenome	)
	// cross over genomes could become a list at some point in the future. i.e. if we were to evolve more than just the 
	// weights and topologies but also learning rates or functions.
	case class AgentResults(crossOverGenomes: NetworkGenome, sse: Double )
}

class Population extends Actor with ActorLogging {
import Population._


	def receive = {

		case PopulationSettings(n, g) =>

			1.to(n).foreach(i => {
				val e = context.actorOf(Props[Experience], "experience" + i)
				context.actorOf(Agent.props(g, e), "agent"+ i)
				})
			context become generations(List.empty, n)

	}



	def generations(agentsComplete: List[AgentResults], totalAgents: Int): Receive = {

		case Network.Matured(genome, error) =>

			if (agentsComplete.length + 1 == totalAgents){
				// start the cross over process
				println("generation complete, get ready for crossover...")
				val finalAgentsComplete = AgentResults(genome, error) :: agentsComplete
				val groupedByPerformance: List[List[AgentResults]] = finalAgentsComplete.sortWith((a,b) => a.sse < b.sse ).grouped(2).toList
				//groupedByPerformance.foreach(x=> x.foreach(y => println(y.sse)))
				val t = groupedByPerformance.map(g=> {
					 	crossover(g)
					})

				val tMutate = t.map(g=> {
					mutateAddConn(g)
					})
				//println(t)


			} else {
				context become generations(AgentResults(genome, error) :: agentsComplete, totalAgents)
			}


	}


	// CROSSOVER
	// we could move this to the agents... or perhaps pass it as a function to them...
	// benefit being it would get done in parallel in large populations this matters.
	// crossover in NEAT actually refers specifically to the connections. Though the Neruons are obviously involved...
	// the neurons for the new genome can be retrieved by a pass through the new connection genome.
	// but which do we take?? In general the Neruons wiht the same innovation number should both be the same
	// however I have added Bias to the neuron itself meaning during learning/mutation they will diverge. Equally perhaps i tis interesting
	// to include mutation of the activation function. Could take neuron
	// randomly or that of the fittest.

	def crossover(g: List[AgentResults]): NetworkGenome = {

		//println("here")
		val networkGenome1 = g(0).crossOverGenomes // Due to the sorting and how the partition data will work. this will always be the strongest.
		val networkGenome2 = g(1).crossOverGenomes

		val crossedConnections: HashMap[Int, ConnectionGenome] = networkGenome1.connections.foldLeft(HashMap[Int, ConnectionGenome]()) { (m, c) =>


			val matching = networkGenome2.connections(c._1)
			if (matching != None) {
				// Randomly take one or other of the genomes.
				m + (List(c, (matching.innovationId -> matching))(Random.nextInt(2)))
			} else {
				// This is the stronger genome take its additional parts. discard the other.
				// TODO: Do we not even want to consider the weaker disjoints or excesss genes. in sharpNeat this appears to be toggled
				// at one point (though commented out) even randomly..
				m + c
			}
			}

		val newGenomes = crossedConnections.foldLeft(HashMap[Int, NeuronGenome]()) { (m, n) =>

			m + (n._2.from -> networkGenome1.neurons(n._2.from), n._2.to -> networkGenome1.neurons(n._2.to) )

		  }
		println(crossedConnections)
		println(newGenomes)


		new NetworkGenome(networkGenome1.neurons, crossedConnections)

	}

	// MUTATIONS
	// Genrally it seems mutation does not get applied until cross over has happened...

	// Perturb weights
		// find a connection
		// vary its weight slightly. or a lot. or by something....

	// Add connection
		// <decscription> find two nodes without a connection
		// add the connection.
		// <param> genome -> the genome due to be mutated.
		// <return> genome -> A new copy of the genome with a new connection added.


	// add node Or splice
	// find connection between two nodes. deactive connection, replace with two new connections and a node


	// add node
	// pick two random nodes. and add a new connected node.

	def mutateAddConn (genome: NetworkGenome) = {
		1 
	}


	// <param> NetworkGenome: the network genome to be mutated
	// <return> NetworkGenome: A genome with a new neuron added.

	def mutateAddNeuron (genome: NetworkGenome) = {
		val connectionToSplit = genome.connections.keys(Random.nextInt(genome.connections.size))
		innovation ! Innovation.NewNeuronProposal(connectionToSplit.from, connectionToSplit.to)
	}
}
