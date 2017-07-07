package com.neurocoevo.genome


import scala.xml.XML
import scala.util.Random

// SHould be able to create a network Genome based on a number of options... 
// xml file, or, layer definition
// Most likely going to be called by the agent.

object GenomeFactory {
	
	def createGenome(filepath: String): NetworkGenome = {

		val xmlData = XML.loadFile(filepath)
		 
		val neurons: List[NeuronGenome] = (xmlData \ "neurons" \ "neuron").map {i => 
				new NeuronGenome( (i \ "@id").text.toInt,
								  (i \ "@activationFuntion").text,
								  (i \ "@type").text,
								  (i \ "@bias").text.toInt ) } .toList

		val connections: List[ConnectionGenome] = (xmlData \ "connections" \ "connection").map {i => 
				new ConnectionGenome( (i \ "@id").text.toInt,
								  (i \ "@src-id").text.toInt,
								  (i \ "@tgt-id").text.toInt,
								  {
								  	val x = (i \ "@weight").text
								  	if(x.length == 0){
								  		Random.nextDouble
								  	} else {
								  		x.toDouble 
								  	}
								  })} .toList
	
		
		new NetworkGenome(neurons, connections)

		
	}

	def createGenome(substrate: Substrate, cppn: Network): NetworkGenome = {}



	// TODO: Does not currently provide correct Id's for the Neurons

	def createGenome(inputs: Int, hidden: List[Int], outputs: Int): NetworkGenome = {
		
		val inputList: List[NeuronGenome] = 1.to(inputs).toList.map(i => new NeuronGenome(i, "Sigmoid", "input", 0))
		val hiddenList: List[List[NeuronGenome]] = hidden.map(l => 1.to(l).toList.map(l => new NeuronGenome(l, "Sigmoid", "hidden", -1) ))
		val outputList: List[NeuronGenome] = 1.to(outputs).toList.map(o => new NeuronGenome(o, "Sigmoid", "output", -1))
		
		val combinedNeuronList: List[List[NeuronGenome]] = inputList :: hiddenList.flatten :: outputList :: List.empty
		val connections = connectionGen(combinedNeuronList)
		new NetworkGenome(combinedNeuronList.flatten, connections)
	}

	private def connectionGen(layerConfiguration: List[List[NeuronGenome]]) : List[ConnectionGenome] = {
		val layers: List[List[NeuronGenome]] = layerConfiguration
		connectionGenAux(layers.lift(0).get, layers.lift(1).get, List.empty, layers.lift(1).get, layers.drop(2))
	}

	private def connectionGenAux(
		from: List[NeuronGenome], 
		to: List[NeuronGenome], 
		connections: List[ConnectionGenome], 
		staticOutputs: List[NeuronGenome],
		toConnect: List[List[NeuronGenome]]): List[ConnectionGenome] = {
			
		if(from.length == 0 && toConnect.length == 0 ){
			// we've iterated through the whole input list. and there're no subsequent layers to connect
			connections		
		} else {
			if(from.length == 0 && toConnect.length > 0 ){
				// made all connections for this layer, but there is still more layers to connect, move to the next layer.
				connectionGenAux(staticOutputs, toConnect.head, connections, toConnect.head, toConnect.tail)	
			} else {
				if(to.length > 0){
					// add a connection from first input to current first in the output list, go to next output.
					connectionGenAux(from, to.tail, new ConnectionGenome(1,from.head.innovationId, to.head.innovationId, Random.nextDouble) :: connections, staticOutputs, toConnect)
				}  else {
					// move to the next input, reset the output list.
					connectionGenAux(from.tail, staticOutputs , connections, staticOutputs, toConnect)
				}
			}
		}
	}

}