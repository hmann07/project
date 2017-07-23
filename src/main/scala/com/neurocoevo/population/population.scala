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
				val e = context.actorOf(Props[Experience], "experience" + i)
				context.actorOf(Agent.props(g, e), "agent"+ i)
				})
			context become generations(List.empty, n, 1, 0)
	}



	def generations(agentsComplete: List[AgentResults], totalAgents: Int, generationNumber: Int, totalError: Double): Receive = {

		case Network.Matured(genome, error) =>

			// DO we kill the children now? Are they at the end of there lifespan?
			// It feels wasteful to close them all down and create new ones.


			// this is sort of a generation over point. we should create new, kill old. and get ready for new AgentResults
			// coming from the new generation.
			if (agentsComplete.length + 1 == totalAgents){
				// start the cross over process
				println("completed generation: #" + generationNumber + " pop: " + totalAgents + " total error: " + totalError)

				val finalAgentsComplete = AgentResults(genome, error, sender()) :: agentsComplete

				// this is likely to be a bottleneck.
				//val groupedByPerformance: List[List[AgentResults]] = finalAgentsComplete.sortWith((a,b) => a.sse < b.sse ).grouped(2).toList
				//groupedByPerformance.foreach(x=> x.foreach(y => println(y.sse)))
				

				// Send to the strongest actor (arbitrary) 2 genomes and a function for crossing them
				// Sooooo
				// need to pick two genomes, then send them off to the agents to be crossed over.
				// Coommon way to do this is roulette wheel.

				finalAgentsComplete.foreach(a => {
					val parent1 = RouletteWheel.select(finalAgentsComplete, totalError)
					val parent2 = RouletteWheel.select(finalAgentsComplete, totalError)
					a.agent ! Crossover(List(parent1, parent2).sortWith((a,b) => a.sse < b.sse ), crossover)
				})

				//val t = groupedByPerformance.map(g=> {
				//	 	g(0).agent ! Crossover(g, crossover)

					 	// Could do anything there, maybe double up the good ones and leave them with slightly differnt values.
					 	//g(1).agent ! Crossover(g, crossover)
				//	})

				// Handle odd number of agents?
				context become spawning(totalAgents, List.empty, generationNumber)

				//val tMutate = t.map(g=> {
				//	mutateAddNeuron(g)
				//	})
				//println(t)


			} else {
				context become generations(
					AgentResults(genome, error, sender()) :: agentsComplete, 
					totalAgents, 
					generationNumber,
					totalError + error)
			}

		
		




	}

	def spawning(expectedChildren: Int, childrenRegistered: List[Agent.NewChild], generationNumber: Int): Receive = {

		case Agent.NewChild(g, name) =>
			if(expectedChildren == childrenRegistered.length + 1) {
				
				(Agent.NewChild(g, name) :: childrenRegistered).foreach( nc => {
				val e = context.actorOf(Props[Experience], "experience" + nc.name)
				context.actorOf(Agent.props(nc.genome, e), nc.name)
				})
				context become generations(List.empty, expectedChildren, generationNumber + 1, 0) 
			} else {
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

				val networkGenome1 = g(0).crossOverGenomes // Due to the sorting and how the partition data will work. this will always be the strongest.
				val networkGenome2 = g(1).crossOverGenomes

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
