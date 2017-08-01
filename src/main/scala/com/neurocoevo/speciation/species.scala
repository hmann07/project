package com.neurocoevo.speciation

import com.neurocoevo.genome.NetworkGenome
import scala.collection.immutable.HashMap

case class Species(
	val champion: NetworkGenome = null,
	val members: HashMap[Int, SpeciesMember] = null,
	val membersCount: Int = 0, // May as well calc here to avoid calling length on the list all the time.
	val speciesTotalFitness: Double = 0,
	val speciesMeanFitness: Double = 0,
	val targetSize: Double = 0

)
