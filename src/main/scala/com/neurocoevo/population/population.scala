package com.neurocoevo.population

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}
import com.neurocoevo.agent.Agent
import com.neurocoevo.experience._

import com.neurocoevo.genome._	

object Population {
	case class PopulationSettings(n: Int, g: NetworkGenome)
}

class Population extends Actor with ActorLogging {
import Population._


	def receive = {

		case PopulationSettings(n, g) =>
			println("hello")	//1.to(n).
			1.to(n).foreach(i => {
				val e = context.actorOf(Props[Experience], "experience" + i)
				context.actorOf(Agent.props(g, e), "agent"+ i)
				})
	}

	// MUTATIONS

	// Perturb weights
		// find a connection
		// vary its weight slightly. or a lot. or by something....

	// Add connection
		// find two nodes without a connection
		// add the connection.

	// add node
		// find connection between two nodes. deactive connection, replace with two new connections and a node

}