package com.neurocoevo.evolution

import com.neurocoevo.genome._
import com.neurocoevo.population._

import scala.util.Random

object RouletteWheel {

	/* 
		select will take in the genomes based on a random number throw and using a cumulative sum
		of error will pick a genome.
	*/
	def select(genomes:List[Population.AgentResults], totalError: Double): Population.AgentResults = {
		val t = totalError * Random.nextDouble
		selectAux(genomes, t, 0)

	}

	def selectAux(genomes:List[Population.AgentResults], errorTarget: Double, cumulativeError: Double): Population.AgentResults = {
		if(errorTarget < cumulativeError + genomes.head.sse){
			genomes.head
		} else {
			selectAux(genomes.tail, errorTarget, cumulativeError + genomes.head.sse)
		}
	}
}