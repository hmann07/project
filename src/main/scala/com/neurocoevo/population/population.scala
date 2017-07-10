package com.neurocoevo.population

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}
import com.neurocoevo.agent.Agent
import com.neurocoevo.experience._
import com.neurocoevo.network._

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
				val sortedByPerformance = finalAgentsComplete.sortWith((a,b) => a.sse > b.sse )

			} else {
				context become generations(AgentResults(genome, error) :: agentsComplete, totalAgents)
			}

			
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