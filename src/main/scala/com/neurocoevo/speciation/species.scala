package com.neurocoevo.speciation

import com.neurocoevo.genome.NetworkGenome

case class Species(
	val champion: NetworkGenome = null,
	val members: List[NetworkGenome] = null,
	val speciesTotalFitness: Double = 0
)
