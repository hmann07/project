package com.neurocoevo.genome

case class NetworkGenome(val neurons: List[NeuronGenome],
					val connections: List[ConnectionGenome]){
	val inputNodes: List[NeuronGenome] = neurons.filter(n => n.neuronType == "input")
	val outputNodes: List[NeuronGenome] = neurons.filter(n => n.neuronType == "output")
	val hiddenNodes: List[NeuronGenome] = neurons.filter(n => n.neuronType == "hidden")
}