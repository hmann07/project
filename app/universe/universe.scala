package com.neurocoevo.universe

import java.io.File
import java.io.PrintWriter
import java.io.FileOutputStream

import com.neurocoevo.genome.GenomeFactory
import com.neurocoevo.innovation.Innovation
import com.neurocoevo.population.Population
import com.neurocoevo.population.PopulationOutput
import com.neurocoevo.network.NetworkOutput
import com.neurocoevo.genome.NetworkGenome
import com.neurocoevo.population.Population.AgentResults
import com.neurocoevo.parameters._

import scala.util.Random

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}

object Universe {

	case class Migrant(val genome: NetworkGenome) 

	case class NoMigrants()

}

class Universe extends Actor with ActorLogging {
	import Universe._
	import context._

	val params = UniverseParameters()

	val runnum = System.currentTimeMillis()

	outputParams(runnum) 

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

	def runningUniverse(bestGenome: AgentResults = null, numBestReceived: Int = 0, waitingPopulations: List[ActorRef] = List.empty) : Receive = {


		// received when the population is ready to receive migrants. 
		case "ready" =>

			// append sender 
			val newWaiting = sender() :: waitingPopulations
			
			// assumption: If we have heard from all, we must also have all the best agents..
			if(newWaiting.length == params.populationCount){
			
				// Then this is the last population so we can send to all
				if (Random.nextDouble < params.migrationRate) {
					context.children.foreach(c => c ! Migrant(bestGenome.genome))
				} else {
					context.children.foreach(c => c ! Migrant(null))
				}
				
			} else {
				
				// we are waiting to hear from more populations.

				context become runningUniverse(bestGenome, numBestReceived, sender() :: waitingPopulations)
			}
			
			

		case Population.AgentResults(genome, sse, fitnessValue, agent, annGenome) =>

			val universeBest = {if(bestGenome != null){
								if(fitnessValue < bestGenome.fitnessValue){
									bestGenome
								}else{
									Population.AgentResults(genome, sse, fitnessValue, agent, annGenome)
								}
							} else {
								Population.AgentResults(genome, sse, fitnessValue, agent, annGenome)
							}}


			context become runningUniverse(universeBest, numBestReceived + 1, waitingPopulations)

	}

	def outputParams(runnum: Long) = {

		val content = Map(
			"speciation" -> SpeciationParameters(),
			"offspring" -> OffspringParameters(),
			"mutation" -> MutationFunctionParameters(),
			"universe" -> UniverseParameters()

			)


		val jsonString = content.map(c => "\"" + c._1 + "\": " + c._2 + "").mkString("{", ", ",  "}")

			val params = OutputParameters()
			val pw = new PrintWriter(new FileOutputStream(new File(params.popOutputPath + "-" + runnum + "-parameters.js"), true))

			try {
				
				pw.append(jsonString +"\r\n") 
				
			}
			finally pw.close()

	}

}