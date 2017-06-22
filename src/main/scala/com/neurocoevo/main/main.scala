package com.neurocoevo.main

import com.neurocoevo.substrate._

object Main extends App {
	val cppnSubstrate = new Substrate("C:\\Users\\Henry\\Downloads\\akka-quickstart-scala\\neurocoevo\\src\\resources\\cppnSubstrate.xml")
	val annSubstrate = new Substrate("C:\\Users\\Henry\\Downloads\\akka-quickstart-scala\\neurocoevo\\src\\resources\\cppnSubstrate.xml")
	cppnSubstrate.inputNodes.foreach(n =>  println(n.dim))
	cppnSubstrate.hiddenNodes.foreach(n =>  println(n.name))
	cppnSubstrate.outputNodes.foreach(n =>  println(n.name))
}