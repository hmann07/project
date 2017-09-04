package com.neurocoevo.genome

import com.neurocoevo.substrate._

/*
	A class representing the nodes and connections of a network (CPPN or ANN). 
	Signature: Substrate -> Genome
	Purpose: Taking in the substrate the genome should create a list of all posible connections between the nodes.
	TODO: come up with a way of dealing with hidden...
*/

class Genome(val substrate: Substrate,
	val inputNodes: List[SubstrateNode],
	val outputNodes: List[SubstrateNode],
	val hiddenNodes: List[List[SubstrateNode]]) {

	val connections: List[Connection] = { connectionGen(substrate) }
	/*
		ConnectionGen: Substrate -> List of Connections.
		Purpose: Taking the input and output nodes and create a list of objects that represent the
				 connections between each of these nodes. 
	*/
	def connectionGen(substrate: Substrate) : List[Connection] = {
		val layers: List[List[SubstrateNode]] = substrate.inputNodes +: substrate.hiddenNodes :+ substrate.outputNodes
		connectionGenAux(layers.lift(0).get, layers.lift(1).get, List.empty, layers.lift(1).get, layers.drop(2))
	}

	def connectionGenAux(
		from: List[SubstrateNode], 
		to: List[SubstrateNode], 
		connections: List[Connection], 
		staticOutputs: List[SubstrateNode],
		toConnect: List[List[SubstrateNode]]): List[Connection] = {
			
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
					connectionGenAux(from, to.tail, new Connection(from.head, to.head) :: connections, staticOutputs, toConnect)
				}  else {
					// move to the next input, reset the output list.
					connectionGenAux(from.tail, staticOutputs , connections, staticOutputs, toConnect)
				}
			}
		}
	}
}