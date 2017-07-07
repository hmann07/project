package com.neurocoevo.main

import com.neurocoevo.substrate._
import com.neurocoevo.genome._
import com.neurocoevo.agent.Agent
import com.neurocoevo.experience.Experience

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}


object Main extends App {
	
	val networkGenome = GenomeFactory.createGenome("C:\\Users\\Henry\\Downloads\\akka-quickstart-scala\\neurocoevo\\src\\resources\\annSubstrate.xml")

	networkGenome.inputNodes.foreach(n =>  println(n.innovationId))

	//val annSubstrate2 = GenomeFactory.createGenome(2,List(2), 1)
	println(networkGenome)
	
	val system = ActorSystem("mySystem")
	val inbox = Inbox.create(system)

	val e = system.actorOf(Props[Experience], "experienceX")
	val agent = system.actorOf(Agent.props(networkGenome, e), "agentX")

	/*
	

	val cppnSubstrate = new Substrate("C:\\Users\\Henry\\Downloads\\akka-quickstart-scala\\neurocoevo\\src\\resources\\annSubstrate.xml")
	val annSubstrate = new Substrate("C:\\Users\\Henry\\Downloads\\akka-quickstart-scala\\neurocoevo\\src\\resources\\annSubstrate.xml")
	cppnSubstrate.inputNodes.foreach(n =>  println(n.dim))
	cppnSubstrate.hiddenNodes.foreach(l => l.foreach(n  =>  println(n.name)))
	cppnSubstrate.outputNodes.foreach(n =>  println(n.name))

	val g = new Genome(cppnSubstrate)
	println(g.connections)
	g.connections.foreach(c => println(c.weight))

	val system = ActorSystem("mySystem")
	val inbox = Inbox.create(system)

	val e = system.actorOf(Props[Experience], "experienceX")
	val agent = system.actorOf(Agent.props(g, e), "agentX")

	inbox.send(agent, "10")
	*/

}