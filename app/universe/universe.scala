package com.neurocoevo.universe

import com.neurocoevo.genome.GenomeFactory
import com.neurocoevo.innovation.Innovation
import com.neurocoevo.population.Population
import com.neurocoevo.population.PopulationOutput
import com.neurocoevo.network.NetworkOutput
import com.neurocoevo.genome.NetworkGenome
import com.neurocoevo.parameters.UniverseParameters

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}

object Universe {

}

class Universe extends Actor with ActorLogging {

	import context._

	val params = UniverseParameters()

	val runnum = System.currentTimeMillis()

	val populations = 1.to(params.populationCount).map(p => {
	
			val networkGenome = GenomeFactory.createGenome(Population.PopulationSettings().genomePath, 0)

			val inn = system.actorOf(Innovation.props(networkGenome), "innovation" + p)

			// start an outputter for the system.
			val networkOutput = system.actorOf(Props[NetworkOutput], "networkOutput" + p)

			val populationOutput = system.actorOf(Props[PopulationOutput], "populationOutputter" + p)

			// start the population actor
			
			val pop = context.actorOf(Population.props(inn, networkOutput, populationOutput), "population" + p)

			

			pop ! Population.PopulationSettings(runNumber = runnum)

			pop
		})
		

	def receive = runningUniverse() 

	def runningUniverse(bestGenome: NetworkGenome = null) : Receive = {

		case "test" =>

		case Population.AgentResults(genome, sse, fitnessValue, agent, annGenome) =>


	}
}