package com.neurocoevo.network

import com.neurocoevo.genome._
import com.neurocoevo.neuron._
import com.neurocoevo.substrate.SubstrateNode

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}

object Network {

	case class NetworkSettings(
			confirmedConnections: Int = 0
		)

	def props(genome: Genome): Props = Props(new Network(genome))

}

class Network(genome: Genome) extends Actor with ActorLogging {
	import context._
	import Network._

	// create actors for nodes
	val inputs: Map[String, ActorRef] = generateNeurons(genome.substrate.inputNodes, Map.empty)
	val outputs: Map[String, ActorRef] = generateNeurons(genome.substrate.outputNodes, Map.empty)
	val allnodes: Map[String, ActorRef] = inputs ++ outputs
	val totalConnections: Int = genome.connections.length 

	// create connections based on actor references
	var actorReferencedConnections = genome.connections.map {c => new ActorConnection(allnodes(c.source.name),allnodes(c.destination.name), c.weight)}

	// inform all neurons about their incoming and outgoing connections.
	actorReferencedConnections.foreach {c =>
		c.source ! Neuron.Destination(Map(c.destination -> c.weight))
		c.destination ! Neuron.Source(Map(c.source -> c.weight))
	}

	println(actorReferencedConnections)
	/*
  		Generate Neurons: Take a list of SubstrateNodes and create actors for each one.
  	*/

  	def generateNeurons(neurons: List[SubstrateNode], agg: Map[String, ActorRef]  ): Map[String, ActorRef] = {
  		neurons.length match {
  			case 0 => agg
  			case _ => generateNeurons(neurons.tail, agg + (neurons.head.name -> actorOf(Props[Neuron], neurons.head.name)))
  		}
  	}

  	def receive = initialisingNetwork(NetworkSettings())

  	// State: initialising. Will be waiting for confirmation from all neurons that they have received their
  	// connections.

  	def	initialisingNetwork(settings: NetworkSettings): Receive = {
    	
    	case "ConnectionConfirmation"  => {
    			if(settings.confirmedConnections == totalConnections){
    				println("received confirmation of all Connections")
    				context become initialisingNetwork(settings.copy(confirmedConnections = settings.confirmedConnections + 1))
    			} else {
    				println("received confirmation of Connection")
    				context become initialisingNetwork(settings.copy(confirmedConnections = settings.confirmedConnections + 1))
    			}
    		}


  	}


}