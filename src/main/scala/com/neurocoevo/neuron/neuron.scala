package com.neurocoevo.neuron

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}

object Neuron {

	val defaultActivationFunction = (x: Double)  => x
	val defaultSignal = 0
	val defaultSignalsRecieved: Double = 0
	val defaultOutputs: Map[ActorRef, Double] = Map.empty
	val defaultInput: Map[ActorRef, Double] = Map.empty

	case class Destination(destination: Map[ActorRef, Double])
	case class Source(source: Map[ActorRef, Double])
	case class NeuronSettings(
    	activationFunction: (Double => Double) = defaultActivationFunction, 
    	signal: Double = defaultSignal,
    	outputs: Map[ActorRef, Double] = defaultOutputs,
    	inputs: Map[ActorRef, Double] = defaultInput,
    	signalsReceived: Double = defaultSignalsRecieved)
    
}


/* Agent. 
		Agent will have a genome that defines a CPPN.
		using the Genome it must set up the actors for the network set up the connections and 
		

*/

class Neuron extends Actor with ActorLogging {
	import context._
	import Neuron._

	println("Neuron created")

	def receive = initialisingNeuron(NeuronSettings())

    def initialisingNeuron(settings: NeuronSettings): Receive = {

    	case Destination(d) => 
    			sender() ! "ConnectionConfirmation"
    			context become initialisingNeuron(settings.copy(outputs = settings.outputs ++ d))

    	case Source(s) => 
    			sender() ! "ConnectionConfirmation"
    			context become initialisingNeuron(settings.copy(inputs = settings.inputs ++ s))
  	}
}