package com.neurocoevo.substrate


import scala.xml.XML


/* 
	SubstrateNode a class for holding the data relevant to a node belonging to a substrate. 
    Signature: ONE xml input/output/hidden, String -> SubstrateNode
    Purpose: return a representation of a node in a substrate with access to it's geometric location
			constructor will iterate over dims of the input constructing a multidimensional vector
 	given
 		<dim>1</dim><dim>2</dim> , node1
 	
 	should return SubstrateNode with name= "node1", dim = Vector(1, 2) 

*/

class SubstrateNode(val nodeXML: scala.xml.NodeSeq, val name: String){
	
	val dim = (nodeXML \ "dim").map {d => d.text }.toList

}
