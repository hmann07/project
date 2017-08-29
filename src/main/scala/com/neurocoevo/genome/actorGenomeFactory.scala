package com.neurocoevo.genome


import scala.xml.XML
import scala.util.Random

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}

import com.neurocoevo.agent._
import com.neurocoevo.substrate._
import com.neurocoevo.network._
import scala.collection.immutable.HashMap


// In the case of HyperNeat, a cppn needs to be queried, meaning messages will need to be sent and handled. It is probably
//  better to have this logic here rather than as part of the agent or Network
//

object ActorGenomeFactory {

	def props(annSubstratePath: String): Props = Props(new ActorGenomeFactory(annSubstratePath))


}

class ActorGenomeFactory(annSubstratePath: String) extends Actor with ActorLogging {
	import ActorGenomeFactory._

	// first create the neuron genomes from the substrate.

	val neurons = createNeuronGenomes(annSubstratePath)

	// Now create dummy neuron for calculating the bias, this neuron will not exist as such since we are storing weights inside the neurons.


	// cross product idea taken from https://stackoverflow.com/questions/14740199/cross-product-in-scala
	// we can assume the order will always be the same and hence all genome connections will get the same id's.. though in HyperNeat this is less important for the ANN.
	// we will not send connections back to inputs.

	val crossed = for { x <- neurons.values.toList; y <- neurons.values.toList.filter(n => n.neuronType != "input") } yield (x, y)

	// kick off the first connection.
	val neuronPair = crossed.head
	context.actorSelection("../cppn") ! Network.Sensation(1, neuronPair._1.location.toList ::: neuronPair._2.location.toList, List(0), "ANNCONFIG")

	def receive = collectingConnections(1, neuronPair, crossed.tail, HashMap.empty)


	def collectingConnections(currentConnectionId: Int, currentPair: (NeuronGenome,NeuronGenome), neuronPairs: List[(NeuronGenome,NeuronGenome)], connectionMap: HashMap[Int, ConnectionGenome]): Receive = {



		case Network.NetworkOutput(output) =>

			val weight = output(5)

			val newConnections = {

				// only express connection if above a certain weight:
				// What happens if we never create a complete network? How do we know if we have expressed connections that will take signal from input to output?

				if(Math.abs(weight) > 0.0 && currentPair._1.layer < currentPair._2.layer) {

				 	connectionMap + (currentConnectionId -> new ConnectionGenome(
									currentConnectionId,
									currentPair._1.innovationId,
								  	currentPair._2.innovationId,
								  	weight * 5 , // Profile the weight between a defined weight range
								  	true, // enabled by default
								    { currentPair._1.layer >= currentPair._2.layer}))

				} else {

					// return connections unaltered

					connectionMap

				}
			}


			if(neuronPairs.isEmpty) {
				//println("completed connection definitions")


				// completed connections so we should now update the neuron bias.

				// work out which ones have bias.

				val biasedNeurons = neurons.values.toList.filter(n => n.neuronType != "input")

				// send first
				val coordinates: List[Double] = (Vector.fill(biasedNeurons.head.location.size)(0.0) ++ biasedNeurons.head.location).toList
				//println(coordinates)

				context.actorSelection("../cppn") ! Network.Sensation(1, coordinates, List(0), "ANNCONFIG")

				context become updatingBias(newConnections, biasedNeurons.head, biasedNeurons.tail, neurons)


			} else {

				//println(context.parent.path.name + "send next neuron")
				val nextNeuronPair = neuronPairs.head
				context.actorSelection("../cppn") ! Network.Sensation(1, nextNeuronPair._1.location.toList ::: nextNeuronPair._2.location.toList, List(0), "ANNCONFIG")
				context become collectingConnections(currentConnectionId + 1, nextNeuronPair, neuronPairs.tail, newConnections)

			}


	}

	def updatingBias(newConnections: HashMap[Int, ConnectionGenome], currentNeuron: NeuronGenome, neurons: List[NeuronGenome], updatedNeurons: HashMap[Int, NeuronGenome]): Receive = {

		case Network.NetworkOutput(outputs) =>

			val bias = outputs(6) * 5 // profile weight into a range.

			val updatedNeuronList = updatedNeurons + (currentNeuron.innovationId -> currentNeuron.copy(biasWeight = bias))

			if(neurons.isEmpty) {
				val finalGenome = new NetworkGenome(0, updatedNeuronList, newConnections)

				//println(finalGenome)
				// TODO: Currently setting genome id to 0, should get this off the agent
				context.parent ! HyperNeatAgent.ConfiguredNetwork(finalGenome)

			} else {

				val coordinates: List[Double] = (Vector.fill(neurons.head.location.size)(0.0) ++ neurons.head.location).toList
				context.actorSelection("../cppn") ! Network.Sensation(1, coordinates, List(0), "ANNCONFIG")
				context become updatingBias(newConnections, neurons.head, neurons.tail, updatedNeuronList)

			}

	}

	def createNeuronGenomes(annSubstratePath: String) = {

		val xmlData = XML.loadFile(annSubstratePath)

		val neurons = (xmlData \ "neurons" \ "neuron").foldLeft(HashMap[Int, NeuronGenome]()) {(neuronList, currentNeuron) =>

				neuronList + ((currentNeuron \ "@id").text.toInt ->

				new NeuronGenome( (currentNeuron \ "@id").text.toInt,
								  (currentNeuron \ "@activationFunction").text,
								  (currentNeuron \ "@type").text,
								  (currentNeuron \ "@bias").text.toInt,
								  {
									val x = (currentNeuron \ "@biasWeight").text
									if(x.length == 0){
										(Random.nextDouble * 2) - 1
									} else {
										x.toDouble
									}
								  },
								  (currentNeuron \ "@layer").text.toDouble,
								  (currentNeuron \ "dim").foldLeft(Vector[Double]())((location, current) => {
									location :+ (current.text).toDouble
								  })
								  ))}
		neurons

	}

}
