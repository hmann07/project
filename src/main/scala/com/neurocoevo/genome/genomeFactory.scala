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

}