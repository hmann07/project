package com.neurocoevo.neuron

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}

object Neuron {

	val defaultActivationFunction = (x: Double)  => x
	val defaultSignal = 0
	val defaultSignalsRecieved: Double = 0
	val defaultOutputs: Map[ActorRef, Double] = Map.empty
	val defaultInput: Map[ActorRef, Double] = Map.empty

	case class Signal(s: Double)
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

    	case "Init Complete" =>
    			println(self.path.name + " neuron ready")
    			println(self.path.name + " inputs: " + settings.inputs)
    			println(self.path.name + " outputs: " + settings.outputs)
    			context become readyNeuron(settings)

    	case Destination(d) => 
    			sender() ! "ConnectionConfirmation"
    			context become initialisingNeuron(settings.copy(outputs = settings.outputs ++ d))

    	case Source(s) => 
    			sender() ! "ConnectionConfirmation"
    			context become initialisingNeuron(settings.copy(inputs = settings.inputs ++ s))
  	}

  	def readyNeuron(settings: NeuronSettings): Receive = {
  		case Signal(v) =>
  			handleSignal(settings, v)
  	}



  	def handleSignal(s: NeuronSettings,v: Double) = {
  	if (s.signalsReceived + 1 == s.inputs.size){
    		println("hidden got all sigs")
	    	s.outputs.keys.foreach(n =>
	    		n ! Signal(s.activationFunction(v) * s.outputs(n)))
	    	context become readyNeuron(s.copy(signal = s.signal + v, signalsReceived = s.signalsReceived + 1))
    	
    	} else {
    		println("hidden got a new sig")
    		println(s.inputs.keys.size)
    		context become readyNeuron(s.copy(signal = s.signal + v, signalsReceived = s.signalsReceived + 1))
    	}
  } 
}

class InputNeuron() extends Neuron {
	import Neuron._
	override def handleSignal(s: NeuronSettings,v: Double) = {
  		println("input got all sigs")
	    	s.outputs.keys.foreach(n =>
	    		n ! Signal(s.activationFunction(v) * s.outputs(n)))
	    	context become readyNeuron(s.copy(signal = s.signal + v, signalsReceived = s.signalsReceived+1))
  	} 
}

class OutputNeuron() extends Neuron {
	import Neuron._
	override def handleSignal(s: NeuronSettings,v: Double) = {
		if (s.signalsReceived + 1 == s.inputs.size){
	  		println("output got all sigs")
		    println("output = " + (s.signal + v))
		    context become readyNeuron(s.copy(signal = s.signal + v, signalsReceived = s.signalsReceived+1))
		} else {
			println("output got new sig")
			context become readyNeuron(s.copy(signal = s.signal + v, signalsReceived = s.signalsReceived+1))
		}
  	} 
}