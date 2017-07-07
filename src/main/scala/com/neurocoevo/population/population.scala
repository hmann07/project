package com.neurocoevo.population

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}
import com.neurocoevo.agent.Agent

import com.neurocoevo.genome._	

object Population {
	case class PopulationSettings(n: Int, g: NetworkGenome, e: ActorRef)
}

class Population extends Actor with ActorLogging {
import Population._


	def receive = {

		case PopulationSettings(n, g, e) =>
			println("hello")	//1.to(n).
			1.to(n).foreach(i =>
				context.actorOf(Agent.props(g, e), "agent"+ i))
	}

}