package com.neurocoevo.main

import com.neurocoevo.substrate._
import com.neurocoevo.genome._
import com.neurocoevo.agent.Agent
import com.neurocoevo.experience.Experience
import com.neurocoevo.population.Population
import com.neurocoevo.innovation.Innovation

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}


object Main extends App {

	//val networkGenomePath = GenomeFactory.createGenome("C:\\Users\\Henry\\Downloads\\akka-quickstart-scala\\neurocoevo\\src\\resources\\annSubstrate.xml")
	//val networkGenomePath = GenomeFactory.createGenome("C:\\Users\\user\\Documents\\project\\project\\src\\resources\\annSubstrate.xml")
	val networkGenomePath = "C:\\Users\\HMann\\Desktop\\project-master (5)\\project-master\\src\\resources\\annSubstrate.xml"

	val system = ActorSystem("mySystem")
	val inbox = Inbox.create(system)


	// Need to give the structure of the genome that is going to be used to seed the population..
	val networkGenome = GenomeFactory.createGenome(networkGenomePath, 0)

	val inn = system.actorOf(Innovation.props(networkGenome), "innovation")

	val p = system.actorOf(Props[Population], "population")

	// innovation needs substrate so that it can choose the right number to start from



	inbox.send(p, Population.PopulationSettings(150,networkGenomePath))

	//val agent = system.actorOf(Agent.props(networkGenome, e), "agentX")
	//val agent2 = system.actorOf(Agent.props(networkGenome, e), "agentY")
	//val agent3 = system.actorOf(Agent.props(networkGenome, e), "agentZ")
	//val agent4 = system.actorOf(Agent.props(networkGenome, e), "agentA")

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
