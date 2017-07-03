package com.neurocoevo.neuron

import com.neurocoevo.network._
import com.neurocoevo.activationfunction._
import scala.util.Random
import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}

object Neuron {

	val defaultActivationFunction = new Sigmoid
	val defaultSignal = 0
	val defaultSignalsRecieved: Map[ActorRef, Double] = Map.empty
	val defaultOutputs: Map[ActorRef, Double] = Map.empty
	val defaultInput: Map[ActorRef, Double] = Map.empty
	//val defaultBias: Double = Network.networkRandom

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
    	totalErrorGradient: Double = 0,
    	biasValue: Double = -1,
    	biasWeight: Double = (Random.nextDouble * 2) - 1)



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
    			println("DestConf")
    			sender() ! "ConnectionConfirmation"
    			context become initialisingNeuron(settings.copy(outputs = settings.outputs ++ d))

    	case Source(s) =>
    			println("sourceConf")
    			sender() ! "ConnectionConfirmation"
    			context become initialisingNeuron(settings.copy(inputs = settings.inputs ++ s))
  	}

  	def readyNeuron(settings: NeuronSettings): Receive = {
  		case Signal(v) =>

  			handleSignal(settings, v, sender())

  		case Network.Error(e) =>

  			handleError(settings, e, sender)

  		case "snapshot" =>
  			sender() ! settings
  	}



  	def handleSignal(s: NeuronSettings,v: Double, source: ActorRef) = {
  		if (s.signalsReceived.size + 1 == s.inputs.size){
    		println(self.path.name + " bias is" + s.biasWeight)

    		val activation = s.activationFunction.function((s.signal + v) + (s.biasWeight * s.biasValue))

	    	s.outputs.keys.foreach(n =>
	    		n ! Signal(activation * s.outputs(n)))

	    	context become readyNeuron(s.copy(signal = s.signal + v,
	    									  signalsReceived = s.signalsReceived + (source -> v)))

    	} else {
    		//println("hidden got a new sig")
    		context become readyNeuron(s.copy(signal = s.signal + v,
    										  signalsReceived = s.signalsReceived + (source -> v)))
    	}
  	}

  	def handleError(s: NeuronSettings, e: Double, source: ActorRef) = {
  		//e is the errorgradient coming from the sender.
  		// we need to update the weight to that sender

		// we need to calculate error gradient for this node. We will have to wait for all error gradients to arrive.
		println(self.path.name + " received error gradient: " + e)
		// We.ve got all gradients can continue propagating
		if(s.errorGradientsReceived + 1 == s.outputs.size){
			val dWeight = s.learningRate * s.activationFunction.function(s.signal + (s.biasValue * s.biasWeight)) * e

			val finalErrorGradient = s.activationFunction.derivative(s.signal + (s.biasValue * s.biasWeight)) * (s.totalErrorGradient + (e * s.outputs(source)))
			val dBiasWeight = s.learningRate * s.biasValue * finalErrorGradient

			println(self.path.name + " final error gradient: " + finalErrorGradient)
			s.inputs.keys.foreach(i =>
  			i ! Network.Error(finalErrorGradient))


			val updatedWeight = (source -> (s.outputs(source) + dWeight))
			val updatedOutputs: Map[ActorRef,Double] = s.outputs + updatedWeight
  			//println("propagated, weights out now: " + nm )
  		println(self.path.name + " new connection weights : " + updatedOutputs )
			// we can also reset the node. ready for incoming forward signals
			println(self.path.name + " new bias: " + (s.biasWeight + dBiasWeight))

			context become readyNeuron(s.copy(signal = 0,
											  signalsReceived = Map.empty,
											  totalErrorGradient = 0,
											  errorGradientsReceived = 0,
											  outputs = updatedOutputs,
											  biasWeight = s.biasWeight + dBiasWeight))

		} else {

			// We've not got all gradients yet. carry on waitng.
			// But update the sum of the error gradient, and update the weight to the output.
			val dWeight = s.learningRate * s.activationFunction.function(s.signal + (s.biasValue * s.biasWeight)) * e
		    val errorGradient =  e * s.outputs(source)
		    val updatedWeight = (source -> (s.outputs(source) + dWeight))
			val updatedOutputs: Map[ActorRef,Double] = s.outputs + updatedWeight
		    // we can update the weight now... won't be using it again.

			context become readyNeuron(s.copy(totalErrorGradient = s.totalErrorGradient + errorGradient,
											  errorGradientsReceived = s.errorGradientsReceived + 1,
											  outputs = updatedOutputs))
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
	    									  signalsReceived = s.signalsReceived + (source -> v),
	    									  biasValue = 0))
  	}

  	override def handleError(s: NeuronSettings, e: Double, source: ActorRef) = {
  		//super.handleError(s,e,source)
			println(self.path.name + " got error gradient " + e)
  		if(s.errorGradientsReceived + 1 == s.outputs.size){
  			val dWeight = s.learningRate * s.signal * e
  			val updatedWeight = (source -> (s.outputs(source) + dWeight))
				val updatedOutputs: Map[ActorRef,Double] = s.outputs + updatedWeight
				println(self.path.name + " new connection weights : " + updatedOutputs )
  			parent ! "propagated"

  			context become readyNeuron(s.copy(signal = 0,
											  signalsReceived = Map.empty,
											  totalErrorGradient = 0,
											  errorGradientsReceived = 0,
											  outputs = updatedOutputs))
  		} else {

  			val dWeight = s.learningRate * s.signal * e
  			val updatedWeight = (source -> (s.outputs(source) + dWeight))
			val updatedOutputs: Map[ActorRef,Double] = s.outputs + updatedWeight

  			context become readyNeuron(s.copy(errorGradientsReceived = s.errorGradientsReceived + 1,
											  outputs = updatedOutputs))

  		}


  		// In addition to appling error gradient to the weights the input should inform the parent that signal has
  		// made it all the way to this end point.
  		// the parent will be waitng to hear from all inputs.


  	}
}

class OutputNeuron() extends Neuron {
	import Neuron._
	import context._

	override def handleSignal(s: NeuronSettings,v: Double, source: ActorRef) = {
		if (s.signalsReceived.size + 1 == s.inputs.size){
	  		val activation = s.activationFunction.function(s.signal + v + (s.biasWeight * s.biasValue))
	  		//println("output got all sigs")
		    //println("output = " + activation)
				println(self.path.name + " bias is" + s.biasWeight)
		    parent ! Network.Output(activation)
		    context become readyNeuron(s.copy(signal = s.signal + v, signalsReceived = s.signalsReceived + (source -> v)))
		} else {
			//println("output got new sig")
			context become readyNeuron(s.copy(signal = s.signal + v, signalsReceived = s.signalsReceived + (source -> v)))
		}
  	}

  	override def handleError(s: NeuronSettings, e: Double, source: ActorRef) = {
  		val errorGradient = s.activationFunction.derivative(s.signal + (s.biasValue * s.biasWeight)) * e

			println("Output error: " + errorGradient)

  		s.inputs.keys.foreach(i =>
  			i ! Network.Error(errorGradient))

  		val dBiasWeight = s.learningRate * errorGradient * s.biasValue
  		// can reset node:
  		println(self.path.name + " new bias: " + (s.biasWeight + dBiasWeight))

  		context become readyNeuron(s.copy(signal = 0,
											  signalsReceived = Map.empty,
											  totalErrorGradient = 0,
											  errorGradientsReceived = 0,
											  biasWeight = s.biasWeight + dBiasWeight))
  	}
}
