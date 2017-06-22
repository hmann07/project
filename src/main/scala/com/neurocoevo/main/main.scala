package com.neurocoevo.main

import com.neurocoevo.substrate._

object Main extends App {
	val t = new Substrate("C:\\Users\\Henry\\Downloads\\akka-quickstart-scala\\akka-quickstart-scala\\src\\resources\\cppnSubstrate.xml")
	t.inputNodes.foreach(n =>  println(n.dim))
	t.hiddenNodes.foreach(n =>  println(n.name))
	t.outputNodes.foreach(n =>  println(n.name))
}