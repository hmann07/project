package com.neurocoevo.agent

import com.neurocoevo.genome._
import com.neurocoevo.neuron._
import com.neurocoevo.substrate.SubstrateNode
import com.neurocoevo.network._

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}

object Agent {

	def props(cppnGenome: Genome): Props = Props(new Agent(cppnGenome))

}


/* Agent. 
		Agent will have a genome that defines a CPPN.
		using the Genome it must set up the actors for the network set up the connections and 
		

*/

class Agent(cppnGenome: Genome) extends Actor with ActorLogging {
	import context._

	println("actor created")

	val cppn = actorOf(Network.props(cppnGenome), "cppn")

	
	def receive = {
    	case "10"  => {
    			sender() ! "10"
    			println("10!")
    		}

    	case "NetworkReady" =>
    		sender() ! Network.Sensation(1, List(1, 0), List(1))

    	case "propagated" =>
    		sender() ! Network.Sensation(1, List(1, 0), List(1))
  	}



  	

}