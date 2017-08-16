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

import scala.util.Random
import scala.collection.immutable.HashMap

import com.neurocoevo.genome._

object Population {

	case class PopulationSettings(
			populationSize: Int = PopulationParameters().populationSize ,
			genomePath: String = PopulationParameters().genomePath,
			agentType: String = PopulationParameters().agentType,
			altGenomePath: String = PopulationParameters().altGenomePath,
			speciationThreshold: Double = SpeciationParameters().speciationThreshold)


	// cross over genomes could become a list at some point in the future. i.e. if we were to evolve more than just the
	// weights and topologies but also learning rates or functions.
	case class AgentResults(genome: NetworkGenome, sse: Double, fitnessValue: Double, agent: ActorRef)

	// All mutation operators are given an id to give to the children.. This id is track by the population so that no agent in the population can have the same.

	case class Crossover(g: List[SpeciesMember], f: (List[SpeciesMember], Int) => NetworkGenome, genomeNumber: Int)

	case class Mutate(genome: NetworkGenome, genomeNumber: Int)

	case class Elite(genome: NetworkGenome, genomeNumber: Int)

	case class SpeciesDirectoryEntry(champion: SpeciesMember, previousChampion: SpeciesMember, actor: ActorRef, totalFitness: Double, memberCount: Double)

	case class SelectParents(totalMeanfitnessValue: Double, populationSize: Int, settings: OffspringParameters)

}

class Population extends Actor with ActorLogging {
import Population._


	def receive = {

		case PopulationSettings(n, genomePath, "STD", altGenomePath,  speciationThreshold) =>

			val agentType = "STD"

			1.to(n).foreach(i => {

				val g = GenomeFactory.createGenome(genomePath, i)

				val e = context.actorOf(Props[Experience], "experience." + i)
				context.actorOf(Agent.props(g, e), "agent."+ i)
				})

			// TODO: is it better to make the outputter a member of the netowrk or of the system or of the agent... rather than population?
			// network doesn't get a view of the ANN.. at the moment.

			context.actorOf(Props[NetworkOutput], "networkOutput")

			context become evolving(PopulationSettings(n,  genomePath, agentType, altGenomePath, speciationThreshold), n, List.empty, n, 1, 0)

		case PopulationSettings(n, cppnGenomePath, "HYPER", annGenomePath, speciationThreshold) =>

			val agentType = "HYPER"

	   		1.to(n).foreach(i => {

				// Here the path has to be passed to the factory so that it initialises the CPPN first then when ready creates the
				// ANN network genome using the weights generated by the CPPN. it could be that all genome generation is done by
				// the agent directly from the paths, however not sure how this works in distributed environment.

	   			 val cppn = GenomeFactory.createGenome(cppnGenomePath, i)

	   			 val e = context.actorOf(Props[Experience], "experience." + i)

				 context.actorOf(HyperNeatAgent.props(cppn, annGenomePath, e), "hyperneatagent."+ i)

			})

	   		 // TODO: is it better to make the outputter a member of the netowrk or of the system or of the agent... rather than population?

			 context.actorOf(Props[NetworkOutput], "networkOutput")

	   		 context become evolving(PopulationSettings(n,  cppnGenomePath, agentType, annGenomePath, speciationThreshold), n, List.empty, n, 1, 0)
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


		// Matches when an agent has processed a set of patterns form the environment.
		case Agent.Matured(genome, fitnessValue, sse, speciesIdx) =>

			// println("matured agent..")

			// this is sort of a generation over point. we should create new, kill old. and get ready for new AgentResults
			// coming from the new generation.
			if (agentsComplete.length + 1 == totalAgents){
				//println("all agents complete")
				// check the population best.
				val best = {if(bestGenome != null){if(sse > bestGenome.sse){bestGenome}else{AgentResults(genome, sse, fitnessValue, sender())}}else {AgentResults(genome, sse, fitnessValue, sender())}}

				// calc final values
				val finalAgentsComplete = (AgentResults(genome, sse, fitnessValue, sender()) :: agentsComplete).sortWith((a,b) => a.sse < b.sse )
				val finalFitnessValue = totalfitnessValue + fitnessValue

				println(generationNumber + ", " + speciesDirectory.size + ", " + totalAgents + ", " + finalFitnessValue + ", " + best.fitnessValue + ", " + best.sse + ", " + best.genome.neurons.size + ", " + best.genome.connections.size)

				//println(best.genome)

				// tell the outputter to save down the best genome in JSON format.
				context.actorSelection("networkOutput") ! NetworkOutput.OutputRequest(best.genome, "PopulationBest" , "JSON")

				// Work out which species each genome is most compatible with.

				val newSpeciesDirectory = checkBestSpecies(genome, fitnessValue, speciesIdx, speciesDirectory, settings.speciationThreshold)


				// need to get the sum of the mean fitnesses for each species. This is used to work out how many offspring each species should create
				// i.e. the sum of the shared fitness values.

				val totalMeanfitnessValue =  speciesDirectory.foldLeft(0.0)((acc, species) =>
						acc + species._2.totalFitness / species._2.memberCount
					)


				// all genomes allocated a species. now we tell the species to create their volunteers.
				val finalSpeciesDirectory = speciesDirectory.foldLeft(HashMap[Int, SpeciesDirectoryEntry]())((directory, dirEntry) => {
					dirEntry._2.actor ! SelectParents(totalMeanfitnessValue, settings.populationSize, OffspringParameters())

					// set current champion as the old champion ready for the new genomes to return their results and be tested
					directory + (dirEntry._1 ->  dirEntry._2.copy(previousChampion = dirEntry._2.champion))
				}
					)

				// move to a state where we will wait for species to send proposal for their offspring to create.
				context become speciating(settings, finalAgentsComplete, generationNumber, currentGenomeNumber + 1, speciesDirectory.size, 0, List.empty, finalSpeciesDirectory)


			} else {


				val newSpeciesDirectory = checkBestSpecies(genome, fitnessValue, speciesIdx, speciesDirectory, settings.speciationThreshold)

				context become evolving(
					settings,
					currentGenomeNumber,
					AgentResults(genome, sse, fitnessValue, sender()) :: agentsComplete,
					totalAgents,
					generationNumber,
					totalfitnessValue + fitnessValue,
					{if(bestGenome != null){if(sse > bestGenome.sse){bestGenome}else{AgentResults(genome, sse, fitnessValue, sender())}}else {AgentResults(genome, sse, fitnessValue, sender())}},
					 newSpeciesDirectory
					)
			}

	}

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
			//println("agent ready")

			if(!agentMessages.isEmpty){

			//println("sending " + agentMessages.head)
			sender() ! agentMessages.head

			context become spawning(settings, expectedChildren,  childrenRegistered, generationNumber, currentGenomeNumber, speciesDirectory, agentMessages.tail )

			} else {
			//	println("run out of messages")
			}
		case Agent.NewChild(g, name) =>

			//println("received " + Agent.NewChild(g, name))
			// Have we received all children?
			//println(expectedChildren + ", " + (childrenRegistered.length + 1))

			if(expectedChildren == childrenRegistered.length + 1) {

				// Stop the last agent.
				context stop sender()
				//println("All children received")
				// create new actors
				(Agent.NewChild(g, name) :: childrenRegistered).foreach(c => {


					val e = context.actorOf(Props[Experience], "experience." + c.genome.id)

					settings.agentType match {
						case "STD" =>
							context.actorOf(Agent.props(c.genome, e), "agent."+ c.genome.id)

						case "HYPER" =>
							context.actorOf(HyperNeatAgent.props(c.genome, settings.altGenomePath, e), "hyperneatagent."+ c.genome.id)
					}
				})

				// start evolving again
				//println("start evovling new population")

				context become evolving(settings, currentGenomeNumber, List.empty, expectedChildren, generationNumber + 1, 0, null, speciesDirectory)

			} else {



				// Stop the agent
				sender() ! "ProcessRequest"

				// Wait for rest..
				context become spawning(settings, expectedChildren,  Agent.NewChild(g, name) :: childrenRegistered, generationNumber, currentGenomeNumber, speciesDirectory, agentMessages)
			}
	}

	def speciating(settings: PopulationSettings, finalAgentsComplete: List[AgentResults], generationNumber: Int, currentGenomeNumber: Int, totalSpecies: Int, finishedSpecies: Int, agentMessages: List[Any] , speciesDirectory:HashMap[Int, SpeciesDirectoryEntry]): Actor.Receive = {

		// this will come from the Species. It means a particular species has finished selecting parents for crossover, nominations for mutation or elites.

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

			context stop sender()

			if (totalSpecies == (finishedSpecies + 1)) {

				finalAgentsComplete.foreach(c =>
					c.agent ! "ProcessRequest")
				context become spawning(settings, agentMessages.length, List.empty, generationNumber, currentGenomeNumber, speciesDirectory - speciesId, agentMessages)

			} else {
				 context become speciating(settings, finalAgentsComplete, generationNumber, currentGenomeNumber, totalSpecies - 1, finishedSpecies, agentMessages, speciesDirectory - speciesId)
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
				Crossover(List(p1,p2), crossover, currentGenomeNumber) :: agentMessages,
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



	// CROSSOVER
	// This function will be passed to the agents or the stronger of the agents.
	// benefit being it would get done in parallel in large populations this matters.
	// crossover in NEAT actually refers specifically to the connections. Though the Neruons are obviously involved...
	// the neurons for the new genome can be retrieved by a pass through the new connection genome.
	// but which do we take?? In general the Neruons wiht the same innovation number should both be the same
	// however I have added Bias to the neuron itself meaning during learning/mutation they will diverge. Equally perhaps i tis interesting
	// to include mutation of the activation function. Could take neuron
	// randomly or that of the fittest.


	def crossover(g: List[SpeciesMember], genomeNumber: Int): NetworkGenome = {

		g.length match {
			case 2 => {
				val performanceSortedGenomes = g.sortWith((a,b) => a.fitness > b.fitness )
				val networkGenome1 = performanceSortedGenomes(0).genome // Due to the previous sort this will always be the strongest.
				val networkGenome2 = performanceSortedGenomes(1).genome

				val crossedConnections: HashMap[Int, ConnectionGenome] = networkGenome1.connections.foldLeft(HashMap[Int, ConnectionGenome]()) { (m, c) =>


					val matching = networkGenome2.connections.contains(c._1)



					if (matching) {
						// Randomly take one or other of the genomes.
						val matched = networkGenome2.connections(c._1)
						m + (List(c, (matched.innovationId -> matched))(Random.nextInt(2)))
					} else {
						// This is the stronger genome take its additional parts. discard the other.
						// TODO: Do we not even want to consider the weaker disjoints or excesss genes. in sharpNeat this appears to be toggled
						// at one point (though commented out) even randomly..
						m + c
					}
					}

				val newGenomes = crossedConnections.foldLeft(HashMap[Int, NeuronGenome]()) { (m, n) =>

					m + (n._2.from -> networkGenome1.neurons(n._2.from), n._2.to -> networkGenome1.neurons(n._2.to) )

				  }
				//println(crossedConnections)
				//println(newGenomes)


				new NetworkGenome(genomeNumber, networkGenome1.neurons, crossedConnections)
			}

			case _ => {
				// this is lazy if signleton genome... just return the existing one.. NEAT software actaully mutates it..
				new NetworkGenome(genomeNumber, g(0).genome.neurons, g(0).genome.connections)
			}
		}
	}

	def checkBestSpecies(genome: NetworkGenome, fitnessValue: Double, speciesIdx: Int, speciesDirectory: HashMap[Int, SpeciesDirectoryEntry], speciationThreshold: Double): HashMap[Int, SpeciesDirectoryEntry] =  {

		val speciationParameters = SpeciationParameters()

		if(speciesDirectory.contains(speciesIdx) && genome.compareTo(speciesDirectory(speciesIdx).previousChampion.genome, speciationParameters) < speciationThreshold) {
			// there is a species and the genome is compatible with it.

			val existingEntry = speciesDirectory(speciesIdx)

			// check who the best in species is...
			val newChampion = if(existingEntry.champion.fitness > fitnessValue) existingEntry.champion else SpeciesMember(genome, fitnessValue)

			existingEntry.actor ! SpeciesMember(genome, fitnessValue)

			speciesDirectory + (speciesIdx -> SpeciesDirectoryEntry(
					newChampion,
					existingEntry.previousChampion, // keep previous champion the same... for subsequent comparisons.
					existingEntry.actor,
					existingEntry.totalFitness + fitnessValue,
					existingEntry.memberCount + 1))

		} else {

			val possibleSpecies = speciesDirectory.find( (x: (Int, SpeciesDirectoryEntry)) => genome.compareTo(x._2.previousChampion.genome, speciationParameters) < speciationThreshold )

			possibleSpecies match {

				case Some((i,s)) =>

					val existingEntry = s

					val newChampion = if(existingEntry.champion.fitness > fitnessValue) existingEntry.champion else SpeciesMember(genome, fitnessValue)

					existingEntry.actor ! SpeciesMember(genome, fitnessValue)

					speciesDirectory + (i -> SpeciesDirectoryEntry(
						newChampion,
						existingEntry.previousChampion, // keep previous champion the same... for subsequent comparisons.
						existingEntry.actor,
						existingEntry.totalFitness + fitnessValue,
						existingEntry.memberCount + 1))

				case None =>

					//println(speciesDirectory.keys.toList.size)

					val newSpeciesId = {if(speciesDirectory.isEmpty) 1 else ((speciesDirectory.keysIterator.toList.max) + 1)}

					val newSpeciesActor = context.actorOf(Species.props(newSpeciesId), "species"+ newSpeciesId)

					newSpeciesActor ! SpeciesMember(genome, fitnessValue)

					speciesDirectory + (newSpeciesId -> SpeciesDirectoryEntry(SpeciesMember(genome, fitnessValue),SpeciesMember(genome, fitnessValue), newSpeciesActor, fitnessValue, 1))

			}

		}

	}
}
