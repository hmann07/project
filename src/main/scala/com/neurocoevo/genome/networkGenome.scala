package com.neurocoevo.genome

import scala.collection.immutable.HashMap

case class NetworkGenome(val neurons: HashMap[Int, NeuronGenome],
					val connections: HashMap[Int, ConnectionGenome]){
	val inputNodes: HashMap[Int, NeuronGenome] = neurons.filter(n => n._2.neuronType == "input")
	val outputNodes: HashMap[Int, NeuronGenome] = neurons.filter(n => n._2.neuronType == "output")
	val hiddenNodes: HashMap[Int, NeuronGenome] = neurons.filter(n => n._2.neuronType == "hidden")
}
