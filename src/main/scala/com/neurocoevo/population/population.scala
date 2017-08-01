package com.neurocoevo.population

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}
import com.neurocoevo.agent.Agent
import com.neurocoevo.experience._
import com.neurocoevo.network._
import com.neurocoevo.innovation._
import com.neurocoevo.evolution.RouletteWheel
import com.neurocoevo.speciation.SpeciationParameters
import com.neurocoevo.speciation.Species
import com.neurocoevo.speciation.SpeciesMember

import scala.util.Random
import scala.collection.immutable.HashMap

import com.neurocoevo.genome._

object Population {
	case class PopulationSettings(
			populationSize: Int,
			genomePath: String,
			elitism: Double = 0.2,
			crossoverRate: Double = 0.5,
			mutationRate: Double = 0.5,
			speciationThreshold: Double = 2.5)

	// cross over genomes could become a list at some point in the future. i.e. if we were to evolve more than just the
	// weights and topologies but also learning rates or functions.
	case class AgentResults(genome: NetworkGenome, sse: Double, fitnessValue: Double, agent: ActorRef)

	// All mutation operators are given an id to give to the children..

	case class Crossover(g: List[AgentResults], f: (List[AgentResults], Int) => NetworkGenome, genomeNumber: Int)

	case class Mutate(genome: NetworkGenome, genomeNumber: Int)

	case class Elite(genome: NetworkGenome, genomeNumber: Int)

}

class Population extends Actor with ActorLogging {
import Population._


	def receive = {

		case PopulationSettings(n, genomePath, elitism, crossoverRate, mutationRate, speciationThreshold) =>

			1.to(n).foreach(i => {

				val g = GenomeFactory.createGenome(genomePath, i)

				val e = context.actorOf(Props[Experience], "experience." + i)
				context.actorOf(Agent.props(g, e), "agent."+ i)
				})
			context become evolving(PopulationSettings(n,  genomePath, elitism, crossoverRate, mutationRate, speciationThreshold), n, List.empty, n, 1, 0)
	}

	def evolving(
		settings: PopulationSettings,
		currentGenomeNumber: Int,
		agentsComplete: List[AgentResults],
		totalAgents: Int,
		generationNumber: Int,
		totalfitnessValue: Double,
		bestGenome: AgentResults = null,
		species: HashMap[Int, Species] = HashMap.empty): Receive = {

		case Agent.Matured(genome, fitnessValue, sse, speciesIdx) =>

			// this is sort of a generation over point. we should create new, kill old. and get ready for new AgentResults
			// coming from the new generation.
			if (agentsComplete.length + 1 == totalAgents){

				// start the cross over process


				// check the best.
				val best = {if(bestGenome != null){if(sse > bestGenome.sse){bestGenome}else{AgentResults(genome, sse, fitnessValue, sender())}}else {AgentResults(genome, sse, fitnessValue, sender())}}

				// calc final values

				val finalAgentsComplete = (AgentResults(genome, sse, fitnessValue, sender()) :: agentsComplete).sortWith((a,b) => a.sse < b.sse )
				val finalFitnessValue = totalfitnessValue + fitnessValue


				//println(generationNumber + ", " + totalAgents + ", " + finalFitnessValue + ", " + best.sse + ", " + best.genome.neurons.size + ", " + best.genome.connections.size+ ", " + best.genome.compareTo(finalAgentsComplete(Random.nextInt(totalAgents)).genome, SpeciationParameters(1,1,1)))
				println(generationNumber + ", " + totalAgents + ", " + finalFitnessValue + ", " + best.sse + ", " + best.genome.neurons.size + ", " + best.genome.connections.size)
				//println("completed generation: #" + generationNumber + " pop: " + totalAgents + " fitness: " + finalFitnessValue + " best genome :" + best.sse)

				// Collect Final list of completed agents.

				// Do we need to use the pop total fitness???
				// species.TargetSize = (int)Math.Round((species.MeanFitness / pop.TotalSpeciesMeanFitness) * pop.PopulationSize);
				// where species.MeanFitness = species.TotalFitness / species.Members.Count;
				// and pop.TotalSpeciesMeanFitness = sum(species.meanFitness)

				// update the species totalFitness and average fitness
				val updatedSpecies = {
					if(speciesIdx==0){
						species
					} else {
						//println(species(1).speciesTotalFitness)
				  		species + (speciesIdx -> species(speciesIdx).copy(
				  			speciesTotalFitness = species(speciesIdx).speciesTotalFitness + fitnessValue,
				  			speciesMeanFitness = (species(speciesIdx).speciesTotalFitness + fitnessValue) / (species(speciesIdx).membersCount + 1),
							members = species(speciesIdx).members + (genome.id -> SpeciesMember(genome, fitnessValue))
				  			))
					}}


				// now send off the genomes to be crossed or mutated. Within their species.

	

				val eliteGenomes = math.min(settings.populationSize, settings.populationSize * settings.elitism).toInt
				val crossingGenomes = ((settings.populationSize - eliteGenomes) * settings.crossoverRate).toInt
				val mutatingGenomes = ((settings.populationSize - eliteGenomes) * settings.mutationRate).toInt

				// println(eliteGenomes + ", " + crossingGenomes + ", " +  mutatingGenomes)

				// BOTTLE NECK  roulettewheel is being run twice for all agenets in population.
				// Nothing else is happening. but simple for now...

				val actionsToPerform = List.fill(eliteGenomes)("ELITE") ::: List.fill(crossingGenomes)("CROSS") ::: List.fill(mutatingGenomes)("MUTATE")


				// Create a list of actions and the agents that are going to perfom them
				val actionsZippedWithAgents = actionsToPerform.zip(finalAgentsComplete)

				val actionsPerformed = actionsZippedWithAgents.foldLeft(currentGenomeNumber + 1)((counter, a) => {

					a._1 match {

						case "ELITE" =>
								val elite = finalAgentsComplete.take(1)
								a._2.agent ! Elite(elite(0).genome, counter)

						case "CROSS" =>
								val parent1 = RouletteWheel.select(finalAgentsComplete, finalFitnessValue)
								val parent2 = RouletteWheel.select(finalAgentsComplete, finalFitnessValue)
								a._2.agent ! Crossover(List(parent1, parent2), crossover, counter)

						case "MUTATE" =>
								val parent1 = RouletteWheel.select(finalAgentsComplete, finalFitnessValue)
								a._2.agent ! Mutate(parent1.genome, counter)
					}

					counter + 1

				})

				// CHECK! currently blanking species each iteration...

				context become spawning(settings, totalAgents, List.empty, generationNumber, actionsPerformed, HashMap.empty)

			} else {

				// update the species totalFitness
				val updatedSpecies = {
					if(speciesIdx==0){
						species
					} else {
				  		species + (speciesIdx -> species(speciesIdx).copy(
							speciesTotalFitness = species(speciesIdx).speciesTotalFitness + fitnessValue,
							members = species(speciesIdx).members + (genome.id -> SpeciesMember(genome, fitnessValue))))
					}}

				context become evolving(
					settings,
					currentGenomeNumber,
					AgentResults(genome, sse, fitnessValue, sender()) :: agentsComplete,
					totalAgents,
					generationNumber,
					totalfitnessValue + fitnessValue,
					{if(bestGenome != null){if(sse > bestGenome.sse){bestGenome}else{AgentResults(genome, sse, fitnessValue, sender())}}else {AgentResults(genome, sse, fitnessValue, sender())}},
					 updatedSpecies
					)
			}

	}

	def spawning(settings: PopulationSettings, expectedChildren: Int, childrenRegistered: List[Agent.NewChild], generationNumber: Int, currentGenomeNumber: Int , species: HashMap[Int, Species]): Receive = {

	// TODO: Enable agents to change themselves, rather than creating new ones and killing old ones...

		case Agent.NewChild(g, name) =>

			// Have we received all children?

			if(expectedChildren == childrenRegistered.length + 1) {

				val speciesDesignation = species + speciateAgent(g, species, settings.speciationThreshold, species.size)
				println(speciesDesignation.size)


				// create all the new children.
				/*
				(Agent.NewChild(g, name) :: childrenRegistered).foreach( nc => {
					val e = context.actorOf(Props[Experience], "experience-" + nc.name)
					context.actorOf(Agent.props(nc.genome, e), nc.name)
				})
				*/

				speciesDesignation.foreach( nc => {
					nc._2.members.foreach  { genome =>
						val e = context.actorOf(Props[Experience], "experience-" + genome._2.genome.id)
						context.actorOf(Agent.props(genome._2.genome, e, nc._1), "agent-" + genome._2.genome.id)
						}
					})


				// Stop the last agent.
				context stop sender()

				// start evolving again
				context become evolving(settings, currentGenomeNumber, List.empty, expectedChildren, generationNumber + 1, 0, null, speciesDesignation)

			} else {


				// check speciation of new agent.
				val speciesDesignation = species + speciateAgent(g, species, settings.speciationThreshold, species.size)
				//println(speciesDesignation)

				// Stop the agent
				context stop sender()

				// Wait for rest..
				context become spawning(settings, expectedChildren,  Agent.NewChild(g, name) :: childrenRegistered, generationNumber, currentGenomeNumber, speciesDesignation )
			}
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

	def crossover(g: List[AgentResults], genomeNumber: Int): NetworkGenome = {

		g.length match {
			case 2 => {
				val performanceSortedGenomes = g.sortWith((a,b) => a.sse < b.sse )
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
				// this is lazy if signleton genome... just return the existing one..
				new NetworkGenome(genomeNumber, g(0).genome.neurons, g(0).genome.connections)
			}
		}
	}

	// speciate agent called while the populatation is spawning, i.e. when an agent has create an offspring, this function will be called to put the new offspring in the correct species.

	def speciateAgent(genome: NetworkGenome, species: HashMap[Int,Species], threshold: Double, speciesCounter: Int): (Int, Species) = {

		val newHMIdx = speciesCounter + 1
		//println(newHMIdx)
		if(species.isEmpty) {

			// we have no existing species, add this genome to a new species.
			//println("empty species, adding first at " + newHMIdx)

			(newHMIdx -> Species(genome, HashMap(genome.id -> SpeciesMember(genome, 0) ), 1))

		}
		else if(genome.compareTo(species.head._2.members(species.head._2.members.keys.toList(Random.nextInt(species.head._2.members.size))).genome, SpeciationParameters(1,1,0.4)) < threshold)
		{
			// then we have found a suitable species.
			//println(genome.compareTo(species.head._2.members(Random.nextInt(species.head._2.members.size)), SpeciationParameters(1,1,0.4)))
			//println("found an appropriate species: " + species.head._1)

			(species.head._1 -> species.head._2.copy(members = species.head._2.members + (genome.id -> SpeciesMember(genome, 0)) , membersCount = species.head._2.membersCount + 1 ))

		}
		else if(species.tail.size > 0){
			// there a still some other species this could belong to
			//println("not an appropriate species, thare are still " + species.tail.size )
			speciateAgent(genome, species.tail, threshold, newHMIdx)
		} else {

			//println("run out of options, create a new species at: " +  newHMIdx)
			// there are no more possibilites so we have to create a new species

			(newHMIdx -> Species(genome, HashMap(genome.id -> SpeciesMember(genome, 0) ), 1))

		}

	}
}
