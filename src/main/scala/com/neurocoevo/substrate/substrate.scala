package com.neurocoevo.substrate

import scala.xml.XML


/*
	Substrate is a class that takes as it's main constructor argument a file path to an XML file.
	XML file should be in prespecified "Substrate" format. 
	it will expect at least 2 types of node: <input> and <output>
	optionally there can be <hidden> and <dim> 
*/

class Substrate(filepath: String) {

	private val xmlData = XML.loadFile(filepath)
	val inputNodes: List[SubstrateNode] = (xmlData \ "input").map {i => { new SubstrateNode(i, "input." + i \ "@id")} }.toList
	val outputNodes: List[SubstrateNode] = (xmlData \ "output").map {i => { new SubstrateNode(i, "output." + i \ "@id")} }.toList
	val hiddenNodes: List[List[SubstrateNode]] = (xmlData \ "layer").map {l => { (l \ "hidden") map {i => new SubstrateNode(i, "hidden." + l\ "@id" + "." + i \ "@id")} }.toList }.toList

}