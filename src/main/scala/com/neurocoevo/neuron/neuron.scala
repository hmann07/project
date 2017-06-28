package com.neurocoevo.neuron

import com.neurocoevo.network._
import com.neurocoevo.activationfunction._

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}

object Neuron {

	val defaultActivationFunction = new Sigmoid
	val defaultSignal = 0
	val defaultSignalsRecieved: Map[ActorRef, Double] = Map.empty
	val defaultOutputs: Map[ActorRef, Double] = Map.empty
	val defaultInput: Map[ActorRef, Double] = Map.empty

	case class Signal(s: Double)
	case class Destination(destination: Map[ActorRef, Double])
	case class Source(source: Map[ActorRef, Double])
	case class NeuronSettings(
    	activationFunction: DifferentiableFunction = defaultActivationFunction, 
    	signal: Double = defaultSignal,
    	outputs: Map[ActorRef, Double] = defaultOutputs,
    	inputs: Map[ActorRef, Double] = defaultInput,
    	signalsReceived: Map[ActorRef, Double] = defaultSignalsRecieved,
    	learningRate: Double = 0.1,
    	errorGradientsReceived: Double = 0,
    	totalErrorGradient: Double = 0)

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
  			handleSignal(settings, v, sender())

  		case Network.Error(v) =>
  			println("errror received.")
  			handleError(settings, v, sender)
  	}



  	def handleSignal(s: NeuronSettings,v: Double, source: ActorRef) = {
  		if (s.signalsReceived.size + 1 == s.inputs.size){
    		println("hidden got all sigs")

	    	s.outputs.keys.foreach(n =>
	    		n ! Signal(s.activationFunction.function(s.signal + v) * s.outputs(n)))
	    	context become readyNeuron(s.copy(signal = s.signal + v,
	    									  signalsReceived = s.signalsReceived + (source -> v)))
    	
    	} else {
    		println("hidden got a new sig")
    		context become readyNeuron(s.copy(signal = s.signal + v,
    										  signalsReceived = s.signalsReceived + (source -> v)))
    	}
  	}

  	def handleError(s: NeuronSettings, e: Double, source: ActorRef) = {
  		//e is the errorgradient coming from the sender.
  		// we need to update the weight to that sender
		
		// we need to calculate error gradient for this node. We will have to wait for all error gradients to arrive.
		
		// We.ve got all gradients can continue propagating
		if(s.errorGradientsReceived + 1 == s.outputs.size){
			val dWeight = s.learningRate * s.signal * e
			val finalErrorGradient = s.totalErrorGradient + (e * s.outputs(source)) * s.activationFunction.derivative(s.signal) 
			
			s.inputs.keys.foreach(i =>
  			i ! Network.Error(finalErrorGradient))

			// we can also reset the node. ready for incoming forward signals

			context become readyNeuron(s.copy(signal = 0,
											  signalsReceived = Map.empty,
											  totalErrorGradient = 0,
											  errorGradientsReceived = 0,
											  outputs = s.outputs + (source -> (s.outputs(source) - dWeight))))

		} else {

			// We've not got all gradients yet. carry on waitng.
			// But update the sum of the error gradient, and update the weight to the output.
			val dWeight = s.learningRate * s.signal * e
		    val errorGradient = e * s.outputs(source) * s.activationFunction.derivative(s.signal)

		    // we can update the weight now... won't be using it again.

			context become readyNeuron(s.copy(totalErrorGradient = s.totalErrorGradient + (e * s.outputs(source)),
											  errorGradientsReceived = s.errorGradientsReceived + 1,
											  outputs = s.outputs + (source -> (s.outputs(source) - dWeight))))
		}
  	} 
}

class InputNeuron() extends Neuron {
	import Neuron._
	import context._
	override def handleSignal(s: NeuronSettings,v: Double, source: ActorRef) = {
  		//println("input got all sigs: " + v)
	    	s.outputs.keys.foreach(n =>
	    		n ! Signal(v * s.outputs(n)))
	    	context become readyNeuron(s.copy(activationFunction = new InputFunction,
	    									  signal = s.signal + v,
	    									  signalsReceived = s.signalsReceived + (source -> v)))
  	} 

  	override def handleError(s: NeuronSettings, e: Double, source: ActorRef) = {
  		super.handleError(s,e,source)
  		println("propagated: " + e)
  		// In addition to appling error gradient to the weights the input should inform the parent that signal has 
  		// made it all the way to this end point.
  		// the parent will be waitng to hear from all inputs.

  		parent ! "propagated"
  	}
}

class OutputNeuron() extends Neuron {
	import Neuron._
	import context._

	override def handleSignal(s: NeuronSettings,v: Double, source: ActorRef) = {
		if (s.signalsReceived.size + 1 == s.inputs.size){
	  		val activation = s.activationFunction.function(s.signal + v)
	  		println("output got all sigs")
		    println("output = " + (s.signal + v))
		    parent ! Network.Output(s.signal + v)
		    context become readyNeuron(s.copy(signal = s.signal + v, signalsReceived = s.signalsReceived + (source -> v)))
		} else {
			println("output got new sig")
			context become readyNeuron(s.copy(signal = s.signal + v, signalsReceived = s.signalsReceived + (source -> v)))
		}
  	}

  	override def handleError(s: NeuronSettings, e: Double, source: ActorRef) = {
  		val errorGradient = s.activationFunction.derivative(s.signal) * e
  		s.inputs.keys.foreach(i =>
  			i ! Network.Error(errorGradient))

  		// can reset node:

  			context become readyNeuron(s.copy(signal = 0,
											  signalsReceived = Map.empty,
											  totalErrorGradient = 0,
											  errorGradientsReceived = 0))
  	}
}