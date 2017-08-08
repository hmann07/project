package com.neurocoevo.genome


import scala.xml.XML
import scala.util.Random

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}

import com.neurocoevo.substrate._
import com.neurocoevo.network._
import scala.collection.immutable.HashMap


// In the case of HyperNeat, a cppn needs to be queried, meaning messages will need to be sent and handled. It is probably
//  better to have this logic here rather than as part of the agent or Network
// 

object ActorGenomeFactory {
}

class ActorGenomeFactory extends Actor with ActorLogging {

	def receive = {

		case NeuronLocation(coords1, coords2) =>
			// got two neurons, need to get a weight.
			context.actorSelection("../cppn") ! Network.Sensation(1, coords1.toList ::: coords2.toList, 0, "ANNCONFIG")

			context become calculatingWeights(msgSent: Int, )
	}
}