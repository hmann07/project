package com.neurocoevo.population

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}
import com.neurocoevo.agent.Agent
import com.neurocoevo.experience._
import com.neurocoevo.network._
import com.neurocoevo.innovation._
import com.neurocoevo.evolution.RouletteWheel

import scala.util.Random
import scala.collection.immutable.HashMap

import com.neurocoevo.genome._

object Population {
	case class PopulationSettings(populationSize: Int, g: NetworkGenome	)
	// cross over genomes could become a list at some point in the future. i.e. if we were to evolve more than just the
	// weights and topologies but also learning rates or functions.
	case class AgentResults(crossOverGenomes: NetworkGenome, sse: Double, agent: ActorRef)

	case class Crossover(g: List[AgentResults], f: List[AgentResults] => NetworkGenome)
}

class Population extends Actor with ActorLogging {
import Population._


	def receive = {

		case PopulationSettings(n, g) =>

			1.to(n).foreach(i => {
				val e = context.actorOf(Props[Experience], "experience." + i)
				context.actorOf(Agent.props(g, e), "agent."+ i)
				})
			context become evolving(List.empty, n, 1, 0)
	}

	def evolving(agentsComplete: List[AgentResults], totalAgents: Int, generationNumber: Int, totalError: Double, bestGenome: AgentResults = null): Receive = {

		case Network.Matured(genome, error) =>

			// this is sort of a generation over point. we should create new, kill old. and get ready for new AgentResults
			// coming from the new generation.
			if (agentsComplete.length + 1 == totalAgents){

				// start the cross over process
				val best = {if(bestGenome != null){if(error > bestGenome.sse){bestGenome}else{AgentResults(genome, error, sender())}}else {AgentResults(genome, error, sender())}}

				println("completed generation: #" + generationNumber + " pop: " + totalAgents + " total error: " + totalError + " best genome :" + best.sse)

				// Collect Final list of completed agents.

				val finalAgentsComplete = AgentResults(genome, error, sender()) :: agentsComplete


				// BOTTLE NECK  roulettewheel is being run twice for all agenets in population.
				// Nothing else is happening. but simple for now...

				finalAgentsComplete.foreach(a => {
					val parent1 = RouletteWheel.select(finalAgentsComplete, totalError)
					val parent2 = RouletteWheel.select(finalAgentsComplete, totalError)
					a.agent ! Crossover(List(parent1, parent2), crossover)
				})

				context become spawning(totalAgents, List.empty, generationNumber)

			} else {
				context become evolving(
					AgentResults(genome, error, sender()) :: agentsComplete,
					totalAgents,
					generationNumber,
					totalError + error,
					{if(bestGenome != null){if(error > bestGenome.sse){bestGenome}else{AgentResults(genome, error, sender())}}else {AgentResults(genome, error, sender())}}
					)
			}

	}

	def spawning(expectedChildren: Int, childrenRegistered: List[Agent.NewChild], generationNumber: Int): Receive = {

	// TODO: Enable agents to change themselves, rather than creating new ones and killing old ones...

		case Agent.NewChild(g, name) =>

			// Have we received all children?

			if(expectedChildren == childrenRegistered.length + 1) {

				(Agent.NewChild(g, name) :: childrenRegistered).foreach( nc => {
					val e = context.actorOf(Props[Experience], "experience" + nc.name)
					context.actorOf(Agent.props(nc.genome, e), nc.name)
				})

				// Stop the last agent.
				context stop sender()

				// start evolving again
				context become evolving(List.empty, expectedChildren, generationNumber + 1, 0, null)

			} else {

				// Stop the agent
				context stop sender()

				// Wait for rest..
				context become spawning(expectedChildren,  Agent.NewChild(g, name) :: childrenRegistered, generationNumber )
			}
	}



	// CROSSOVER
	// This function will be passed to the agents or the stronger of the agents.
	// benefit being it would get done in parallel in large populations this matters.
	// crossover in NEAT actually refers specifically to the connections. Though the Neruons are obviously involved...
	// the neurons for the new genome can be retrieved by a pass through the new connection genome.
	// but which do we take?? In general the Neruons wiht the same innovation number should both be the same
	// however I have added Bias to the neuron itself meaning during learning/mutation they will diverge. Equally perhaps i tis interesting
	// to include mutation of the activation function. Could take neuron
	// randomly or that of the fittest.

	def crossover(g: List[AgentResults]): NetworkGenome = {

		g.length match {
			case 2 => {
				val performanceSortedGenomes = g.sortWith((a,b) => a.sse < b.sse )
				val networkGenome1 = performanceSortedGenomes(0).crossOverGenomes // Due to the previous sort this will always be the strongest.
				val networkGenome2 = performanceSortedGenomes(1).crossOverGenomes

				val crossedConnections: HashMap[Int, ConnectionGenome] = networkGenome1.connections.foldLeft(HashMap[Int, ConnectionGenome]()) { (m, c) =>


					val matching = networkGenome2.connections.contains(c._1)



					if (matching) {
						// Randomly take one or other of the genomes.
						val matched = networkGenome2.connections(c._1)
						m + (List(c, (matched.innovationId -> matched))(Random.nextInt(2)))
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
				//println(crossedConnections)
				//println(newGenomes)


				new NetworkGenome(networkGenome1.neurons, crossedConnections)
			}

			case _ => {
				// this is lazy if signleton genome... just return the existing one..
				new NetworkGenome(g(0).crossOverGenomes.neurons, g(0).crossOverGenomes.connections)
			}
		}
	}
}
