package com.neurocoevo.population

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}
import com.neurocoevo.agent.Agent
import com.neurocoevo.agent.HyperNeatAgent
import com.neurocoevo.experience._
import com.neurocoevo.network._
import com.neurocoevo.innovation._
import com.neurocoevo.evolution.RouletteWheel
import com.neurocoevo.parameters._
import com.neurocoevo.speciation.Species
import com.neurocoevo.speciation.SpeciesMember
import com.neurocoevo.universe.Universe.Migrant

import scala.util.Random
import scala.collection.immutable.HashMap

import com.neurocoevo.genome._

object Population {

	def props(innovationAgent: ActorRef, networkOutput: ActorRef, populationOutput: ActorRef): Props = Props(new Population(innovationAgent, networkOutput, populationOutput))


	case class PopulationSettings(
			populationSize: Int = PopulationParameters().populationSize ,
			genomePath: String = PopulationParameters().genomePath,
			agentType: String = PopulationParameters().agentType,
			altGenomePath: String = PopulationParameters().altGenomePath,
			speciationThreshold: Double = SpeciationParameters().speciationThreshold,
			runNumber: Double = 0.0,
			migration: Boolean = PopulationParameters().migrate
			)


	// cross over genomes could become a list at some point in the future. i.e. if we were to evolve more than just the
	// weights and topologies but also learning rates or functions.
	case class AgentResults(genome: NetworkGenome, sse: Double, fitnessValue: Double, agent: ActorRef, annGenome: NetworkGenome = null)

	// All mutation operators are given an id to give to the children.. This id is track by the population so that no agent in the population can have the same.

	case class Crossover(g: List[SpeciesMember], genomeNumber: Int)

	case class Mutate(genome: NetworkGenome, genomeNumber: Int)

	case class Elite(genome: NetworkGenome, genomeNumber: Int)

	case class SpeciesDirectoryEntry(champion: SpeciesMember, previousChampion: SpeciesMember, actor: ActorRef, totalFitness: Double, memberCount: Double)

	case class SelectParents(totalMeanfitnessValue: Double, populationSize: Int, settings: OffspringParameters)





}

class Population(innovationAgent: ActorRef, networkOutput: ActorRef, populationOutput: ActorRef) extends Actor with ActorLogging {
import Population._

	// clean up the innovation tracker for this population

	override def postStop() {
		println(self.path.name + " finished")

		// since we are done with the population all the actor for logging data can be closed down (after they've finished the final bits of data)
		//networkOutput ! akka.actor.PoisonPill
		//populationOutput ! akka.actor.PoisonPill
        //innovationAgent ! akka.actor.PoisonPill
        context.children.foreach {_ ! akka.actor.PoisonPill}
    }





	def receive = {

		case PopulationSettings(n, genomePath, "STD", altGenomePath,  speciationThreshold, rn, migration) =>

			val agentType = "STD"

			1.to(n).foreach(i => {

				val g = GenomeFactory.createGenome(genomePath, i)

				val e = context.actorOf(Props[Experience], "experience." + i)
				context.actorOf(Agent.props(g, e, innovationAgent, agentType), "agent."+ i)

				})

			// TODO: is it better to make the outputter a member of the netowrk or of the system or of the agent... rather than population?
			// network doesn't get a view of the ANN.. at the moment.

			context become evolving(PopulationSettings(n,  genomePath, agentType, altGenomePath, speciationThreshold, rn, migration), n, List.empty, n, 1, 0)

		case PopulationSettings(n, cppnGenomePath, "HYPER", annGenomePath, speciationThreshold, rn, migration) =>

			val agentType = "HYPER"

	   		1.to(n).foreach(i => {

				// Here the path has to be passed to the factory so that it initialises the CPPN first then when ready creates the
				// ANN network genome using the weights generated by the CPPN. it could be that all genome generation is done by
				// the agent directly from the paths, however not sure how this works in distributed environment.

	   			 val cppn = GenomeFactory.createGenome(cppnGenomePath, i)

	   			 val e = context.actorOf(Props[Experience], "experience." + i)

				 context.actorOf(HyperNeatAgent.props(cppn, annGenomePath, e, innovationAgent), "hyperneatagent."+ i)

			})


	   		 context become evolving(PopulationSettings(n,  cppnGenomePath, agentType, annGenomePath, speciationThreshold, rn, migration), n, List.empty, n, 1, 0)


		 case PopulationSettings(n, genomePath, "BP", altGenomePath,  speciationThreshold, rn, migration) =>

			val agentType = "BP"

			1.to(n).foreach(i => {

				val g = GenomeFactory.createGenome(genomePath, i)

				val e = context.actorOf(Props[Experience], "experience." + i)
				context.actorOf(Agent.props(g, e, innovationAgent, agentType), "agent."+ i)

				})

			// TODO: is it better to make the outputter a member of the netowrk or of the system or of the agent... rather than population?
			// network doesn't get a view of the ANN.. at the moment.

			context become evolving(PopulationSettings(n,  genomePath, agentType, altGenomePath, speciationThreshold, rn, migration), n, List.empty, n, 1, 0)

	}

	def evolving(
		settings: PopulationSettings,
		currentGenomeNumber: Int,
		agentsComplete: List[AgentResults],
		totalAgents: Int,
		generationNumber: Int,
		totalfitnessValue: Double,
		bestGenome: AgentResults = null,
		speciesDirectory: HashMap[Int, SpeciesDirectoryEntry] = HashMap.empty): Actor.Receive = {

		case Agent.BackPropagationStats(genome, iteration, timestamp, sse) =>

			val gStats: Map[String, Double] = Map(
					"agent" -> sender().path.name.replace("agent.","").toInt,
					"populationName" -> self.path.name.replace("population","").toInt,
					"epoch" -> iteration,
					"timestamp" -> timestamp,
					"sse" -> sse)

			populationOutput ! PopulationOutput.PopOutputRequest(gStats, "BackPropagationStats", "CSV")
			//println(sender().path.name.replace("agent.","").toInt + ", " + iteration + ", " + timestamp + ", "  + sse)


		case "Ready" =>
			// this must have been a leftover from spawning stage... which should be able to stop it..
			context stop sender()

		// Matches when an agent has processed a set of patterns form the environment.
		case Agent.Matured(genome, fitnessValue, sse, speciesIdx, timestamp, annGenome) =>


			val agentResult = AgentResults(genome, sse, fitnessValue, sender(), annGenome)

			// check the population best.

			val best = {if(bestGenome != null){
								if(fitnessValue < bestGenome.fitnessValue){
									bestGenome
								}else{
									agentResult
								}
							} else {
								agentResult
							}}

			if (agentsComplete.length + 1 == totalAgents){

				// this is sort of a generation over point. we should create new, kill old. and get ready for new AgentResults
				// coming from the new generation.

				// calc final fitness values
				val finalAgentsComplete = (agentResult :: agentsComplete)
				val finalFitnessValue = totalfitnessValue + fitnessValue


				// tell the universe about the champion

				context.parent ! best

				// tell the outputter to save down the best genome in JSON format.

				networkOutput ! NetworkOutput.OutputRequest(best.genome, self.path.name + "-PopulationBest-" + generationNumber , "JSON")

				// if we are in HyperNeat mode
				if(settings.agentType == "HYPER"){
					networkOutput ! NetworkOutput.OutputRequest(best.annGenome, "HyperNeatANN" + generationNumber , "JSON")
				}

				// Work out which species this genome is most compatible with.

				val newSpeciesDirectory = checkBestSpecies(genome, fitnessValue, speciesIdx, speciesDirectory, settings.speciationThreshold)


				// need to get the sum of the mean fitnesses for each species. This is used to work out how many offspring each species should create
				// i.e. the sum of the shared fitness values.

				val totalMeanfitnessValue =  newSpeciesDirectory.foldLeft(0.0)((acc, species) =>

						if(species._2.memberCount > 0 ){

							// There are members of this species

							acc + species._2.totalFitness / species._2.memberCount
						} else {

							// There are no members allocated this generation, it may be extinct.

							acc
						}
					)

				//println(generationNumber + ", " + speciesDirectory.size + ", " + totalAgents + ", " + finalFitnessValue + ", " + best.fitnessValue + ", " + best.sse + ", " + best.genome.neurons.size + ", " + best.genome.connections.size)

				// create a map of items for generation stats
				val gStats: Map[String, Double] = Map(
					"runNumber" -> settings.runNumber,
					"timestamp" -> timestamp,
					"populationName" -> self.path.name.replace("population","").toInt,
					"generationNumber" -> generationNumber,
					"populationSize" -> totalAgents,
					"speciesCount" -> speciesDirectory.size,
					"totalMeanfitnessValue" -> totalMeanfitnessValue,
					"bestGenomeFitness" -> best.fitnessValue ,
					"bestsse" -> best.sse,
					"bestNeuronCount" -> best.genome.neurons.size,
					"bestConnectionCount" -> best.genome.connections.size
					)

				populationOutput ! PopulationOutput.PopOutputRequest(gStats, "populationStats", "JSON")
				populationOutput ! PopulationOutput.PopOutputRequest(gStats, "populationStats", "CSV")


				if(generationNumber == 200){
						// then we've performed the prescribed number of generations


						context.stop(self)
						

				 } else {

					// start work to create new generation

					// all genomes allocated a species. now we tell the species to create their volunteers and update current champion to be previous champion.

					val finalSpeciesDirectory = newSpeciesDirectory.foldLeft(HashMap[Int, SpeciesDirectoryEntry]())((directory, dirEntry) => {

						dirEntry._2.actor ! SelectParents(totalMeanfitnessValue, settings.populationSize, OffspringParameters())

						// set current champion as the old champion ready for the new genomes to return their results and be tested against this previous champion.
						// also reset fitness values counts etc..

						directory + (dirEntry._1 ->  dirEntry._2.copy(
							//champion = null,
							previousChampion = dirEntry._2.champion,
							totalFitness = 0,
							memberCount = 0
							))
					}
						)

					
					// move to a state where we will wait for species to send proposal for their offspring to create.
					context become speciating(
						settings, 
						finalAgentsComplete, 
						generationNumber, 
						currentGenomeNumber + 1, 
						finalSpeciesDirectory.size, 
						0, 
						List.empty, 
						finalSpeciesDirectory
						)
				}
				

			} else {

				// Work out which species this genome is most compatible with.

				val newSpeciesDirectory = checkBestSpecies(genome, fitnessValue, speciesIdx, speciesDirectory, settings.speciationThreshold)

				context become evolving(
					settings,
					currentGenomeNumber,
					AgentResults(genome, sse, fitnessValue, sender()) :: agentsComplete,
					totalAgents,
					generationNumber,
					totalfitnessValue + fitnessValue,
					{if(bestGenome != null){if(fitnessValue < bestGenome.fitnessValue){bestGenome}else{AgentResults(genome, sse, fitnessValue, sender(), annGenome)}}else {AgentResults(genome, sse, fitnessValue, sender(), annGenome)}},
					 newSpeciesDirectory
					)
			}

	}

	// This state runs after the speciation state. While in this moe the population will wait foreach
	// Children, once all children are received create the new agents and go to evolving state.

	def spawning(
		settings: PopulationSettings,
		expectedChildren: Int,
		childrenRegistered: List[Agent.NewChild],
		generationNumber: Int,
		currentGenomeNumber: Int ,
		speciesDirectory: HashMap[Int, SpeciesDirectoryEntry],
		agentMessages: List[Any] ): Actor.Receive = {

	// TODO: Enable agents to change themselves, rather than creating new ones and killing old ones...

		case "Ready" =>


			if(!agentMessages.isEmpty){

			//println("sending " + agentMessages.head)
			sender() ! agentMessages.head

			context become spawning(settings, expectedChildren,  childrenRegistered, generationNumber, currentGenomeNumber, speciesDirectory, agentMessages.tail )

			} else {
			context stop sender()
			}
		
		case Agent.NewChild(g, name) =>

			//println("received " + Agent.NewChild(g, name))
			// Have we received all children?
			//println(expectedChildren + ", " + (childrenRegistered.length + 1))

			if(expectedChildren == childrenRegistered.length + 1) {

				// Stop the last agent.
				context stop sender()	

				// time to send migration

				if(settings.migration) {
					context.parent ! "ready"
					context become awaitMigrants(
						settings, 
						Agent.NewChild(g, name) :: childrenRegistered,
			        	generationNumber,  
						currentGenomeNumber, 
						expectedChildren,
						speciesDirectory)
				
				} else {

					// Just continue.

					// create new actors
					(Agent.NewChild(g, name) :: childrenRegistered).foreach(c => {


						val e = context.actorOf(Props[Experience], "experience." + c.genome.id)

						settings.agentType match {
							case "STD" =>
								context.actorOf(Agent.props(c.genome, e, innovationAgent, "STD"), "agent."+ c.genome.id)

							case "HYPER" =>
								context.actorOf(HyperNeatAgent.props(c.genome, settings.altGenomePath, e, innovationAgent), "hyperneatagent."+ c.genome.id)
						}
					})

					// start evolving again
					
					context become evolving(settings, currentGenomeNumber, List.empty, expectedChildren, generationNumber + 1, 0, null, speciesDirectory)
					
				}

			} else {



				// There are still children to be created by the agent
				sender() ! "ProcessRequest"

				// Wait for rest..
				context become spawning(settings, expectedChildren,  Agent.NewChild(g, name) :: childrenRegistered, generationNumber, currentGenomeNumber, speciesDirectory, agentMessages)
			}
	}


	// this will be caused by the spawning state 
	// When migrant is recieved all new children will be created, then proceed to evolving state.

	def awaitMigrants(
		settings: PopulationSettings, 
		childrenRegistered: List[Agent.NewChild], 
		generationNumber: Int, 
		currentGenomeNumber: Int, 
		expectedChildren: Int,
		speciesDirectory:  HashMap[Int, SpeciesDirectoryEntry] ): Receive = {

		case "Ready" =>
			// this must have been a leftover from spawning stage... which should be able to stop it..
			context stop sender()

		case Migrant(genome) =>

				
					if(genome != null){
						
						val g = genome.copy(id = currentGenomeNumber)

						val e = context.actorOf(Props[Experience], "experience." + currentGenomeNumber)
						settings.agentType match {
								case "STD" =>
									context.actorOf(Agent.props(g, e, innovationAgent, "STD"), "agent."+ currentGenomeNumber)

								case "HYPER" =>
									context.actorOf(HyperNeatAgent.props(g, settings.altGenomePath, e, innovationAgent), "hyperneatagent."+ currentGenomeNumber)
							}
					} 		
					
					val numberOfChildren = {if(genome !=null) {expectedChildren + 1} else {expectedChildren}}



					// create new actors
					childrenRegistered.foreach(c => {


						val e = context.actorOf(Props[Experience], "experience." + c.genome.id)

						settings.agentType match {
							case "STD" =>
								context.actorOf(Agent.props(c.genome, e, innovationAgent, "STD"), "agent."+ c.genome.id)

							case "HYPER" =>
								context.actorOf(HyperNeatAgent.props(c.genome, settings.altGenomePath, e, innovationAgent), "hyperneatagent."+ c.genome.id)
						}
					})

					// start evolving again
					context become evolving(settings, currentGenomeNumber + 1, List.empty,  numberOfChildren, generationNumber + 1, 0, null, speciesDirectory)

	}


	def speciating(settings: PopulationSettings, finalAgentsComplete: List[AgentResults], generationNumber: Int, currentGenomeNumber: Int, totalSpecies: Int, finishedSpecies: Int, agentMessages: List[Any] , speciesDirectory:HashMap[Int, SpeciesDirectoryEntry]): Actor.Receive = {

		// this will come from a Species actor.
		// It means a particular species has finished selecting parents for crossover, nominations for mutation or elites.
		// there may still be more species to hear from.

		case "AllParentsSelected" =>


			if (totalSpecies == (finishedSpecies + 1)) {

				// Then we have heard from all species

				// here we ask all the agents to do some processing for us (i.e. Mutate, Crossover, even echo an Elite. )

				finalAgentsComplete.foreach(c =>
					c.agent ! "ProcessRequest")


				// Move into a state where agents will voluteer themselves and we reply with enough data for them to do the processing

				context become spawning(settings, agentMessages.length, List.empty, generationNumber, currentGenomeNumber, speciesDirectory, agentMessages)

			} else {

				// There are still species to hear from

				 context become speciating(settings, finalAgentsComplete, generationNumber, currentGenomeNumber, totalSpecies, finishedSpecies + 1, agentMessages, speciesDirectory)

				}

		case Species.Extinct(speciesId) =>

			// The species seems to have no members..
			// It should have stopped itself
			//context stop sender()
			val updatedSpeciesDirectory = speciesDirectory + (speciesId -> speciesDirectory(speciesId).copy(totalFitness = 0, memberCount = 0))

			if (totalSpecies == (finishedSpecies + 1)) {
				// Then we have heard from all species

				// here we ask all the agents to do some processing for us (i.e. Mutate, Crossover, even echo an Elite. )
				finalAgentsComplete.foreach(c =>
					c.agent ! "ProcessRequest")

				// Move into a state where agents will voluteer themselves and we reply with enough data for them to do the processing
				// We also change the species from the directory.



				context become spawning(settings, agentMessages.length, List.empty, generationNumber, currentGenomeNumber, updatedSpeciesDirectory, agentMessages)

			} else {

				 // There are still species to hear back from. reduce the total we are due to hear from by 1, and remove it from the directory, leaving finsihed species static.

				 context become speciating(settings, finalAgentsComplete, generationNumber, currentGenomeNumber, totalSpecies, finishedSpecies + 1, agentMessages, updatedSpeciesDirectory)
				}

		case Species.Crossover(p1,p2) =>


			//println("crossover")

			context become speciating(
				settings,
				finalAgentsComplete,
				generationNumber,
				currentGenomeNumber + 1,
				totalSpecies,
				finishedSpecies,
				Crossover(List(p1,p2), currentGenomeNumber) :: agentMessages,
				speciesDirectory
				)

		case Species.Mutate(p1) =>
			//println("mutate")

			context become speciating(
				settings,
				finalAgentsComplete,
				generationNumber,
				currentGenomeNumber + 1,
				totalSpecies,
				finishedSpecies,
				Mutate(p1.genome, currentGenomeNumber) :: agentMessages,
				speciesDirectory)

		case Species.Elite(e1) =>
			//println("elite")

			context become speciating(
				settings,
				finalAgentsComplete,
				generationNumber,
				currentGenomeNumber + 1,
				totalSpecies,
				finishedSpecies,
				Elite(e1.genome, currentGenomeNumber) :: agentMessages,
				speciesDirectory)
	}





	def checkBestSpecies(genome: NetworkGenome, fitnessValue: Double, speciesIdx: Int, speciesDirectory: HashMap[Int, SpeciesDirectoryEntry], speciationThreshold: Double): HashMap[Int, SpeciesDirectoryEntry] =  {

		val speciationParameters = SpeciationParameters()

		if(speciesDirectory.contains(speciesIdx) && genome.compareTo(speciesDirectory(speciesIdx).previousChampion.genome, speciationParameters) < speciationThreshold) {

			// there is a species and this genome is compatible with its champion.

			val existingEntry = speciesDirectory(speciesIdx)

			// double check who the best in species is...
			val newChampion = if(existingEntry.champion.fitness > fitnessValue) existingEntry.champion else SpeciesMember(genome, fitnessValue)

			// inform the species they have a new member
			existingEntry.actor ! SpeciesMember(genome, fitnessValue)

			// update and return an updated directory
			speciesDirectory + (speciesIdx -> SpeciesDirectoryEntry(
					newChampion,
					existingEntry.previousChampion, // keep previous champion the same... for subsequent comparisons.
					existingEntry.actor,
					existingEntry.totalFitness + fitnessValue,
					existingEntry.memberCount + 1))

		} else {

			// the species of the genome was not in the index or the genome is not close enough the the previous generation champion...

			// check  if the genome is close enough to another species

			val possibleSpecies = speciesDirectory.find( (x: (Int, SpeciesDirectoryEntry)) => genome.compareTo(x._2.previousChampion.genome, speciationParameters) < speciationThreshold )

			possibleSpecies match {

				case Some((i,s)) =>

					// then we have found an existing species where this genome is compatible with the champion

					val existingEntry = s

					// does this genome have better fitness?

					val newChampion = if(existingEntry.champion != null) {
											if(existingEntry.champion.fitness > fitnessValue) {
												existingEntry.champion
												} else{
												 SpeciesMember(genome, fitnessValue)
												}
										} else {
											 SpeciesMember(genome, fitnessValue)
										}

					// inform the speices of its new member
					existingEntry.actor ! SpeciesMember(genome, fitnessValue)

					speciesDirectory + (i -> SpeciesDirectoryEntry(
						newChampion,
						existingEntry.previousChampion, // keep previous champion the same... for subsequent comparisons.
						existingEntry.actor,
						existingEntry.totalFitness + fitnessValue,
						existingEntry.memberCount + 1))

				case None =>

					// then this genome was not compatible with any of the existing species so we need to create a new one.

					val newSpeciesId = speciesDirectory.size + 1

					val newSpeciesActor = context.actorOf(Species.props(newSpeciesId), "species"+ newSpeciesId + context.self.path.name)

					newSpeciesActor ! SpeciesMember(genome, fitnessValue)

					speciesDirectory + (newSpeciesId ->
						SpeciesDirectoryEntry(
							SpeciesMember(genome, fitnessValue),
							SpeciesMember(genome, fitnessValue),
							newSpeciesActor,
							fitnessValue,
							1
							))

			}

		}

	}
}
