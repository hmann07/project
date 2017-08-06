
package com.neurocoevo.speciation

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}

import com.neurocoevo.genome.NetworkGenome
import com.neurocoevo.population._
import com.neurocoevo.evolution.RouletteWheel
import scala.collection.immutable.HashMap

object Species {

def props(speciesId: Int): Props = Props(new Species(speciesId))

case class SpeciesSettings(
	val champion: SpeciesMember = null,
	val members: HashMap[Int, SpeciesMember] = HashMap.empty,
	val memberCount: Int = 0, // May as well calc here to avoid calling length on the list all the time.
	val speciesTotalFitness: Double = 0,
	val speciesMeanFitness: Double = 0,
	val targetSize: Double = 0

)

case class Crossover(p1: SpeciesMember, p2: SpeciesMember)
case class Mutate(p1: SpeciesMember)
case class Elite(e: SpeciesMember)
case class Extinct(speciesId: Int)

}

class Species(val speciesId: Int) extends Actor with ActorLogging {
import Species._


	def receive = trackSpecies(SpeciesSettings()) 

	def trackSpecies(s: SpeciesSettings): Receive = {


		case SpeciesMember(genome, fitness) =>

			context become trackSpecies(s.copy(
				champion = if(s.champion == null || s.champion.fitness < fitness) SpeciesMember(genome, fitness) else s.champion ,
				members = s.members + (genome.id -> SpeciesMember(genome, fitness)),
				memberCount = s.memberCount + 1,
				speciesTotalFitness = s.speciesTotalFitness + fitness ,
				speciesMeanFitness = (s.speciesTotalFitness + fitness) / (s.memberCount + 1)
				))



		case Population.SelectParents(popTotalMeanFitness, population, settings) =>

			//println("create")
			
			// we need to check if this species still exists.. it could be that none of the children were compatible with the champion 
			// genome 
			if(s.members.size == 0) {
			
				context.parent ! Extinct(speciesId)
			
			} else {

				val speciesTargetSize = math.round(((s.speciesMeanFitness / popTotalMeanFitness) * population )).toInt
				val eliteGenomes = math.min(s.memberCount, math.floor(speciesTargetSize * settings.elitism).toInt);
				val crossingGenomes = ((speciesTargetSize - eliteGenomes) * settings.crossoverRate).toInt
				val mutatingGenomes = math.max(1, ((speciesTargetSize - eliteGenomes) * settings.mutationRate).toInt) // 
				val offSpringTargets  = (crossingGenomes,mutatingGenomes,eliteGenomes)
				
				//println(speciesTargetSize + ", " + eliteGenomes + ", " +crossingGenomes + ", " + mutatingGenomes)

				selectParents(offSpringTargets, s.members.values.toList.sortWith((a,b) => a.fitness > b.fitness ) , s.speciesTotalFitness, s)
			}	
	}



	def selectParents(targets: (Int, Int, Int), members: List[SpeciesMember], speciesTotalFitness: Double, settings: SpeciesSettings ): Unit = {

		targets match {
			case (0,0,0) =>

				// not sure, guess tell pop that species done...

				context.parent ! "AllParentsSelected"
				context become trackSpecies(settings.copy(members= HashMap.empty, memberCount = 0, speciesTotalFitness= 0, speciesMeanFitness = 0))

			case  (x,y,z) if x > 0 => 

				// cross over
				

				// Analysis of NEAT code seems to suggest that only the stop two are ever selected for mating....
				val parent1 = RouletteWheel.select(members, speciesTotalFitness)
				val parent2 = RouletteWheel.select(members, speciesTotalFitness)

				if(members.length == 1) {
					val parent1 = members(0)
					context.parent ! Mutate(parent1)

				} else {

				//val parent1 = members(0)
				//val parent2 = members(1)

				context.parent ! Crossover(parent1, parent2)

				}
				
				selectParents((x - 1, y, z), members,speciesTotalFitness, settings)

			case (0,y,z) if y > 0 =>

				// mutate
				val parent1 = RouletteWheel.select(members, speciesTotalFitness)


				context.parent ! Mutate(parent1)

				selectParents((0, y - 1, z), members,speciesTotalFitness, settings)
			

			case (0,0,z) if z > 0 =>


				//println(members(0).fitness + ", " + members(members.length - 1).fitness) 

				// allocate elites
				val elites = members.take(z)

				elites.foreach(e =>
					context.parent ! Elite(e)
					)

				selectParents((0, 0, 0), members,speciesTotalFitness, settings)
		}
	}
}