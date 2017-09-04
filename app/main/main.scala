package com.neurocoevo.main

import com.neurocoevo.substrate._
import com.neurocoevo.genome._
import com.neurocoevo.agent.Agent
import com.neurocoevo.experience.Experience
import com.neurocoevo.population.Population
import com.neurocoevo.innovation.Innovation
import com.neurocoevo.parameters._

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}


object Main extends App {

	// Create Actor System.

	val system = ActorSystem("mySystem")
	val inbox = Inbox.create(system)

	// innovation needs to analyse the genome to be evolved so that it can choose the right number to start from

	val networkGenome = GenomeFactory.createGenome(Population.PopulationSettings().genomePath, 0)

	val inn = system.actorOf(Innovation.props(networkGenome), "innovation")

	// Create a population.

	val p = system.actorOf(Props[Population], "population")

	// kick everything off...

	inbox.send(p, Population.PopulationSettings())



}
