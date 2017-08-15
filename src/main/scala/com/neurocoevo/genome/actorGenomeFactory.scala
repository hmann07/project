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

	case class ConnectionWeight(weight: Double)

}

class ActorGenomeFactory(annSubstratePath: String) extends Actor with ActorLogging {
	import ActorGenomeFactory._
	// first create the neuron genomes from the substrate.

	val neurons = createNeuronGenomes(annSubstratePath)

	// cross product idea taken from https://stackoverflow.com/questions/14740199/cross-product-in-scala
	// we can assume the order will always be the same and hence all genome connections will get the same id's.. though in HyperNeat this is less important for the ANN.
	// we will not send connections back to inputs.

	val crossed = for { x <- neurons.values.toList; y <- neurons.values.toList.filter(n => n.neuronType != "input") } yield (x, y)

	// kick off the first connection.
	val neuronPair = crossed.head
	context.actorSelection("../cppn") ! Network.Sensation(1, neuronPair._1.location.toList ::: neuronPair._2.location.toList, List(0), "ANNCONFIG")

	def receive = collectingConnections(1, neuronPair, crossed.tail, HashMap.empty)


	def collectingConnections(currentConnectionId: Int, currentPair: (NeuronGenome,NeuronGenome), neuronPairs: List[(NeuronGenome,NeuronGenome)], connectionMap: HashMap[Int, ConnectionGenome]): Receive = {



		case ConnectionWeight(weight) =>

			//println(weight)
			val newConnections = {

				// only express connection if above a certain weight:
				// What happens if we never create a complete network? How do we know if we have expressed connections that will take signal from input to output?

				if(Math.abs(weight) > 0.0) {

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

				// TODO: Currently setting genome id to 0, should get this off the agent

				val finalGenome = new NetworkGenome(0, neurons, newConnections)

				//println(finalGenome)

				context.parent ! HyperNeatAgent.ConfiguredNetwork(finalGenome)

			} else {

				//println(context.parent.path.name + "send next neuron")
				val nextNeuronPair = neuronPairs.head
				context.actorSelection("../cppn") ! Network.Sensation(1, nextNeuronPair._1.location.toList ::: nextNeuronPair._2.location.toList, List(0), "ANNCONFIG")
				context become collectingConnections(currentConnectionId + 1, nextNeuronPair, neuronPairs.tail, newConnections)

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
