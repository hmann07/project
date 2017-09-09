package com.neurocoevo.neuron

import com.neurocoevo.network._
import com.neurocoevo.activationfunction._
import com.neurocoevo.genome.NeuronGenome
import com.neurocoevo.genome.ConnectionGenome

import scala.util.Random

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}

import scala.collection.immutable.HashMap

object Neuron {

	def props(biasWeight: Double, activationFunction: ActivationFunction ): Props = Props(new Neuron(biasWeight, activationFunction))

	val defaultActivationFunction = ActivationFunction("SIGMOID")
	val defaultAccumulatedSignal = 0
	val defaultSignalsRecieved: Map[ActorRef, Double] = Map.empty
	val defaultOutputs: Map[ActorRef, ConnectionDetail] = Map.empty
	val defaultInput: Map[ActorRef, ConnectionDetail] = Map.empty
	//val defaultBias: Double = Network.networkRandom

	case class ConnectionDetail(id: Int, weight: Double, recurrent: Boolean)
	case class Signal(s: Double, recurrent: Boolean, signalType: String)
	case class Destination(destination: Map[ActorRef, ConnectionDetail])
	case class Source(source: Map[ActorRef, ConnectionDetail])
	case class NeuronSettings(
		activationFunction: ActivationFunction = defaultActivationFunction,
    	accumulatedSignal: Double = defaultAccumulatedSignal,
		activationOutput: Double = 0,
		recurrentAccumulatedSignal: Double = 0,
		accumulateRecurrent: Boolean = false,
    	outputs: Map[ActorRef, ConnectionDetail] = defaultOutputs,
    	inputs: Map[ActorRef, ConnectionDetail] = defaultInput,
    	signalsReceived: Map[ActorRef, Double] = defaultSignalsRecieved,
    	learningRate: Double = 0.1,
    	errorGradientsReceived: Double = 0,
    	totalErrorGradient: Double = 0,
    	biasValue: Int = -1,
    	biasWeight: Double = (Random.nextDouble * 2) - 1)

	case class NeuronSnapshot(g: (Int, NeuronGenome), c: HashMap[Int, ConnectionGenome])


}

object InputNeuron {
	def props(biasWeight: Double, activationFunction: ActivationFunction ): Props = Props(new InputNeuron(biasWeight, activationFunction))
}
object OutputNeuron {
	def props(biasWeight: Double, activationFunction: ActivationFunction): Props = Props(new OutputNeuron(biasWeight, activationFunction))
}


/* Agent.
		Agent will have a genome that defines a CPPN.
		using the Genome it must set up the actors for the network set up the connections and


*/

class Neuron(pBiasWeight: Double, activationFunction: ActivationFunction) extends Actor with ActorLogging {
	import context._
	import Neuron._

	//println("Neuron created")

	def receive = initialisingNeuron(NeuronSettings(biasWeight = pBiasWeight, activationFunction = activationFunction))

    def initialisingNeuron(settings: NeuronSettings): Receive = {

    	case "Init Complete" =>
    			//println(self.path.name + " neuron ready")
    			//println(self.path.name + " inputs: " + settings.inputs)
    			//println(self.path.name + " outputs: " + settings.outputs)
    			context become readyNeuron(settings)

    	case Destination(d) =>
    			//println("DestConf")

				// recurrent connections are treated as any other

    			sender() ! "ConnectionConfirmation"
    			context become initialisingNeuron(settings.copy(outputs = settings.outputs ++ d))

    	case Source(s) =>

				// We need to be specific about inputs to neruon in terms of recurrent connections because
				// we don't want to wait for their signal.
				// we also don't need to worry about back prop for now... since recurrent weights will be learned by evolution

				if(s.head._2.recurrent){

					sender() ! "ConnectionConfirmation"
					// no need to do anything. // not going to count for inputs coming recurrently...
					context become initialisingNeuron(settings)
				} else {
					//println("sourceConf")
					sender() ! "ConnectionConfirmation"
					context become initialisingNeuron(settings.copy(inputs = settings.inputs ++ s))
				}


  	}

  	def readyNeuron(settings: NeuronSettings): Receive = {
  		case Signal(v, recurrent, signalType) =>

  			handleSignal(settings, v, sender(), recurrent, signalType)

  		case Network.Error(e) =>

  			handleError(settings, e, sender)


  		case "Relax" =>
  			relax(settings)
  	}



  	def handleSignal(s: NeuronSettings,v: Double, source: ActorRef, recurrent: Boolean, signalType: String) = {

		// if signal coming in has a reccurent flag then this needs to be stored.

		if(recurrent){
			// if the neuron has recently fired then we have used the accumulated recurrent signals so we can start re-collecting post firing.
			if(!s.accumulateRecurrent) {
				context become readyNeuron(s.copy(
					recurrentAccumulatedSignal  = v,
					accumulateRecurrent = true))
			} else {
				context become readyNeuron(s.copy(
					recurrentAccumulatedSignal  = s.recurrentAccumulatedSignal + v))
			}
		} else {

  		if (s.signalsReceived.size + 1 == s.inputs.size){

    		//println(self.path.name + " bias is" + s.biasWeight)

			// all inputs received. Ready to fire.
			// we have to assume we got all recurrent signals.. if not then the network: fed forward, relaxed and tooka whole new batch of signals to node, before some future node passed it's signal back.
			// which is possible. but for that, perhaps the genome deserves to perform badly.

			val finalAccumalatedSignal = (s.accumulatedSignal + v + s.recurrentAccumulatedSignal) + (s.biasWeight * s.biasValue)
    		val activation = s.activationFunction.function(finalAccumalatedSignal)

	    	s.outputs.keys.foreach(n => {

	    		n ! Signal(activation * s.outputs(n).weight, s.outputs(n).recurrent, signalType )
	    	})


			// logic here is if we are not learning then we can reset the signals now..
			// if learning via back propagation then we need these values to stay in order to calculate error gradients.
			if(signalType == "ANNCONFIG") {
				context become readyNeuron(s.copy(accumulateRecurrent = false, activationOutput = 0, accumulatedSignal = 0, signalsReceived = Map.empty))
			} else {
	    	context become readyNeuron(s.copy(
	    		accumulateRecurrent = false,
				accumulatedSignal  = finalAccumalatedSignal,
				activationOutput = activation,
	    		signalsReceived = s.signalsReceived + (source -> v)))
			}

    	} else {
    		////println("hidden got a new sig")
    		context become readyNeuron(s.copy(accumulatedSignal = s.accumulatedSignal + v,
    										  signalsReceived = s.signalsReceived + (source -> v)))
    	}
    }
  	}

  	def handleError(s: NeuronSettings, e: Double, source: ActorRef) = {
  		//e is the errorgradient coming from the sender.
  		// we need to update the weight to that sender

		// we need to calculate error gradient for this node. We will have to wait for all error gradients to arrive.
		//println(self.path.name + " received error gradient: " + e)
		// We.ve got all gradients can continue propagating
		if(s.errorGradientsReceived + 1 == s.outputs.size){

			val dWeight = s.learningRate * s.activationOutput * e

			val finalErrorGradient = s.activationFunction.derivative(s.accumulatedSignal) * (s.totalErrorGradient + (e * s.outputs(source).weight))
			val dBiasWeight = s.learningRate * s.biasValue * finalErrorGradient

			//println(self.path.name + " final error gradient: " + finalErrorGradient)
			s.inputs.keys.foreach(i =>
  			i ! Network.Error(finalErrorGradient))

			val currentWeight = s.outputs(source).weight
			val updatedWeight = (source -> s.outputs(source).copy(weight = currentWeight + dWeight))
			val updatedOutputs: Map[ActorRef, ConnectionDetail] = s.outputs + updatedWeight
  			//println("propagated, weights out now: " + nm )
  			//println(self.path.name + " new connection weights : " + updatedOutputs )
			// we can also reset the node. ready for incoming forward signals
			//println(self.path.name + " new bias: " + (s.biasWeight + dBiasWeight))

			context become readyNeuron(s.copy(
				accumulatedSignal = 0,
				activationOutput = 0,
				signalsReceived = Map.empty,
			  	totalErrorGradient = 0,
			  	errorGradientsReceived = 0,
			  	outputs = updatedOutputs,
		  		biasWeight = s.biasWeight + dBiasWeight))

		} else {

			// We've not got all gradients yet. carry on waitng.
			// But update the sum of the error gradient, and update the weight to the output.

			val dWeight = s.learningRate * s.activationOutput * e
		    val errorGradient =  e * s.outputs(source).weight
		    val currentWeight = s.outputs(source).weight
			val updatedWeight = (source -> s.outputs(source).copy(weight = currentWeight + dWeight))
			val updatedOutputs: Map[ActorRef, ConnectionDetail] = s.outputs + updatedWeight
		    // we can update the weight now... won't be using it again.

			context become readyNeuron(s.copy(
				totalErrorGradient = s.totalErrorGradient + errorGradient,
				errorGradientsReceived = s.errorGradientsReceived + 1,
				outputs = updatedOutputs))
		}
  	}


  	def relax(s: NeuronSettings) = {

  		sender() ! "NeuronRelaxed"
  		context become readyNeuron(s.copy(accumulatedSignal = 0, signalsReceived = Map.empty, activationOutput = 0))
  	}

}

class InputNeuron(biasWeight: Double, activationFunction: ActivationFunction) extends Neuron(biasWeight: Double, activationFunction: ActivationFunction) {
	import Neuron._
	import context._
	override def handleSignal(s: NeuronSettings,v: Double, source: ActorRef, recurrent: Boolean, signalType: String) = {
  		////println("input got all sigs: " + v)
	    	s.outputs.keys.foreach(n => {
				// inputs should never emit or receive recurrent signals
	    		//println(self.path.name + ", outputs: " + v * s.outputs(n).weight)
	    		//println(self.path.name + ", connweight: " + s.outputs(n).weight)
	    		n ! Signal(v * s.outputs(n).weight, false, signalType)

	    		})

			// logic here is if we are not learning then we can reset the signals now..
			// if learning via back propagation then we need these values to stay in order to calculate error gradients.
			// for inputs this is not really required
			if(signalType == "ANNCONFIG") {
				context become readyNeuron(s.copy(activationOutput = 0, accumulatedSignal = 0, signalsReceived = Map.empty))
			} else {

	    	context become readyNeuron(s.copy(
	    									  accumulatedSignal = s.accumulatedSignal + v,
	    									  signalsReceived = s.signalsReceived + (source -> v),
	    									  biasValue = 0))
			}
  	}

  	override def handleError(s: NeuronSettings, e: Double, source: ActorRef) = {
  		//super.handleError(s,e,source)
			//println(self.path.name + " got error gradient " + e)
  		if(s.errorGradientsReceived + 1 == s.outputs.size){

			val dWeight = s.learningRate * s.accumulatedSignal * e
  			val currentWeight = s.outputs(source).weight
			val updatedWeight = (source -> s.outputs(source).copy(weight = currentWeight + dWeight))
			val updatedOutputs: Map[ActorRef, ConnectionDetail] = s.outputs + updatedWeight
				//println(self.path.name + " new connection weights : " + updatedOutputs )

			// In addition to appling error gradient to the weights the input should inform the parent that signal has
  			// made it all the way to this end point.
  			// the parent will be waitng to hear from all inputs.
  			parent ! "propagated"

  			context become readyNeuron(s.copy(accumulatedSignal = 0,
											  signalsReceived = Map.empty,
											  totalErrorGradient = 0,
											  errorGradientsReceived = 0,
											  outputs = updatedOutputs))
  		} else {

  			val dWeight = s.learningRate * s.accumulatedSignal * e
  			val currentWeight = s.outputs(source).weight
			val updatedWeight = (source -> s.outputs(source).copy(weight = currentWeight + dWeight))
			val updatedOutputs: Map[ActorRef, ConnectionDetail] = s.outputs + updatedWeight

  			context become readyNeuron(s.copy(errorGradientsReceived = s.errorGradientsReceived + 1,
											  outputs = updatedOutputs))

  		}
  	}

}

class OutputNeuron(biasWeight: Double, activationFunction: ActivationFunction) extends Neuron(biasWeight: Double, activationFunction: ActivationFunction) {
	import Neuron._
	import context._

	override def handleSignal(s: NeuronSettings,v: Double, source: ActorRef, recurrent: Boolean, signalType: String) = {

		// in reality there should only ever be one, but it might be that one output node pushes signal to a sibling.
		if(recurrent){
			// if the neuron has recently fired then we have used the accumulated recurrent signals so we can start re-collecting post firing.
			if(!s.accumulateRecurrent) {
				context become readyNeuron(s.copy(
					recurrentAccumulatedSignal  = v,
					accumulateRecurrent = true))
			} else {


				context become readyNeuron(s.copy(
					recurrentAccumulatedSignal  = s.recurrentAccumulatedSignal + v))
			}
		} else {

			if (s.signalsReceived.size + 1 == s.inputs.size){

				//println(" output accumulated signal " + (s.accumulatedSignal + v))
				//println(" recurrent va;: " + s.recurrentAccumulatedSignal)
				//println(" biaswieght " + s.biasWeight)
				//println(" bias " + s.biasValue)

				val finalAccumalatedSignal = (s.accumulatedSignal + v + s.recurrentAccumulatedSignal) + (s.biasWeight * s.biasValue)
				val activation = s.activationFunction.function(finalAccumalatedSignal)

				//println("output got all sigs")
			    //println("output = " + activation)
			    //println(self.path.name + " bias is" + s.biasWeight)
				//println(s.outputs.keys)

				// these should all be recurrent..
				s.outputs.keys.foreach(n => {
					//println(n + ", " + s.outputs(n).recurrent)

		    		n ! Signal(activation * s.outputs(n).weight, s.outputs(n).recurrent, signalType)})

				//println(self.path.name + ", outputs " + activation)
		    	//println(self.path.name + ", acc signal: " + finalAccumalatedSignal)



				parent ! Network.Output(activation, signalType)

				// logic here is if we are not learning then we can reset the signals now..
				// if learning via back propagation then we need these values to stay in order to calculate error gradients.
				if(signalType == "ANNCONFIG") {
					context become readyNeuron(s.copy(accumulateRecurrent = false, activationOutput = 0, accumulatedSignal = 0, signalsReceived = Map.empty))
				} else {

				context become readyNeuron(s.copy(
					accumulateRecurrent = false,
					accumulatedSignal = finalAccumalatedSignal,
					activationOutput = activation,
					signalsReceived = s.signalsReceived + (source -> v)))
				}

		} else {

			//println(" output accumulated signal " + (s.accumulatedSignal + v))

			//println("output got new sig")
			context become readyNeuron(s.copy(

			accumulatedSignal = s.accumulatedSignal + v,
			signalsReceived = s.signalsReceived + (source -> v)))
		}
  	}
}
  	override def handleError(s: NeuronSettings, e: Double, source: ActorRef) = {

		val errorGradient = s.activationFunction.derivative(s.accumulatedSignal) * e

		//println("Output error: " + errorGradient)

  		s.inputs.keys.foreach(i =>
  			i ! Network.Error(errorGradient))

  		val dBiasWeight = s.learningRate * errorGradient * s.biasValue
  		// can reset node:
  		//println(self.path.name + " new bias: " + (s.biasWeight + dBiasWeight))

  		context become readyNeuron(s.copy(
			accumulatedSignal = 0,
			activationOutput = 0,
	  		signalsReceived = Map.empty,
			totalErrorGradient = 0,
			errorGradientsReceived = 0,
			biasWeight = s.biasWeight + dBiasWeight))
  	}
}
