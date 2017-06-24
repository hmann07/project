package com.neurocoevo.genome

import com.neurocoevo.substrate._

/*
	A class representing the nodes and connections of a network (CPPN or ANN). 
	Signature: Substrate -> Genome
	Purpose: Taking in the substrate the genome should create a list of all posible connections between the nodes.
	TODO: come up with a way of dealing with hidden...
*/

class Genome(val substrate: Substrate) {
	val connections: List[Connection] = { connectionGen(substrate) }

	
	/*
		ConnectionGen: Substrate -> List of Connections.
		Purpose: Taking the input and output nodes and create a list of objects that represent the
				 connections between each of these nodes. 
	*/
	def connectionGen(substrate: Substrate) : List[Connection] = {
		connectionGenAux(substrate.inputNodes, substrate.outputNodes, List.empty, substrate.outputNodes)
	}

	def connectionGenAux(inputs: List[SubstrateNode], outputs: List[SubstrateNode], connections: List[Connection], staticOutputs: List[SubstrateNode]): List[Connection] = {
			if(inputs.length == 0){
				connections		
			} else {
				if(outputs.length > 0){
					// add a connection from first input to current first in the output list, go to next output.
					connectionGenAux(inputs, outputs.tail, new Connection(inputs.head, outputs.head) :: connections, staticOutputs)
				}  else {
					// move to the next input, reset the output list.
					connectionGenAux(inputs.tail, staticOutputs , connections, staticOutputs)
				}
			}
		}
}