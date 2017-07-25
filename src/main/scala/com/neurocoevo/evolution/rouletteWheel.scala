package com.neurocoevo.evolution

import com.neurocoevo.genome._
import com.neurocoevo.population._

import scala.util.Random

object RouletteWheel {

	/* 
		select will take in the genomes based on a random number throw and using a cumulative sum
		of error will pick a genome. those with bigger 
	*/
	def select(genomes:List[Population.AgentResults], totalFitnessValue: Double): Population.AgentResults = {
		val t = totalFitnessValue * Random.nextDouble
		selectAux(genomes, t, 0)

	}

	def selectAux(genomes:List[Population.AgentResults], targetFitnessValue: Double, cumulativeError: Double): Population.AgentResults = {
		if(targetFitnessValue < cumulativeError + genomes.head.fitnessValue){
			genomes.head
		} else {
			selectAux(genomes.tail, targetFitnessValue, cumulativeError + genomes.head.fitnessValue)
		}
	}
}