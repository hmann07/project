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
	//val networkGenomePath = "C:\\Users\\user\\Documents\\neurocoevo\\project\\src\\resources\\annSubstrate.xml"
	//val networkGenomePath = "C:\\Users\\Henry\\Downloads\\project-master\\project-master\\src\\resources\\annSubstrate.xml"

	// Create Actor System.

	val system = ActorSystem("mySystem")
	val inbox = Inbox.create(system)


	// Create a population.
	val p = system.actorOf(Props[Population], "population")

	// NEAT
	// val networkGenomePath = "C:\\Users\\HMann\\Desktop\\project-master (9)\\project-master\\src\\resources\\annSubstrate.xml"
	//val networkGenome = GenomeFactory.createGenome(networkGenomePath, 0)
	//inbox.send(p, Population.PopulationSettings(150,networkGenomePath, "STD"))


	// HyperNEAT
	val networkGenomePath = "C:\\Users\\HMann\\Desktop\\project-master (9)\\project-master\\src\\resources\\hyperneatAnnSubstrate.xml"
	val cppnGenomePath = "C:\\Users\\HMann\\Desktop\\project-master (9)\\project-master\\src\\resources\\cppnSubstrate.xml"
	val networkGenome = GenomeFactory.createGenome(cppnGenomePath, 0)
	inbox.send(p, Population.PopulationSettings(150, cppnGenomePath, "HYPER", networkGenomePath))

	// innovation needs substrate so that it can choose the right number to start from

	val inn = system.actorOf(Innovation.props(networkGenome), "innovation")




}
