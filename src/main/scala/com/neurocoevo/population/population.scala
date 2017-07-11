package com.neurocoevo.population

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}
import com.neurocoevo.agent.Agent
import com.neurocoevo.experience._
import com.neurocoevo.network._
import scala.util.Random


import com.neurocoevo.genome._	

object Population {
	case class PopulationSettings(n: Int, g: NetworkGenome	)
	// cross over genomes could become a list at some point in the future.
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
				//println(t)


			} else {
				context become generations(AgentResults(genome, error) :: agentsComplete, totalAgents)
			}

			
	}


	// CROSSOVER
	// we could move this to the agents... or perhaps pass it as a function to them...
	// benefit being it would get done in parallel in large populations this matters.

	def crossover(g: List[AgentResults]): NetworkGenome = {

		//println("here")
		val networkGenome1 = g(0).crossOverGenomes // Due to the sorting and how the partition data will work. this will always be the strongest.
		val networkGenome2 = g(1).crossOverGenomes

		val crossedNeurons: List[NeuronGenome] = networkGenome1.neurons.map { n => 
			val matching = networkGenome2.neurons.find(n2 => n2.innovationId == n.innovationId)
			if (matching.get != None) {
				List(n, matching.get)(Random.nextInt(1))
			} else {
				n
			}
			}.toList
		
		println(crossedNeurons)


		new NetworkGenome(crossedNeurons, networkGenome2.connections)

	}

	// MUTATIONS
	// Genrally it seems mutation does not get applied until cross over has happened...

	// Perturb weights
		// find a connection
		// vary its weight slightly. or a lot. or by something....

	// Add connection
		// find two nodes without a connection
		// add the connection.

	// add node
		// find connection between two nodes. deactive connection, replace with two new connections and a node

}