package com.neurocoevo.evolution

import com.neurocoevo.genome._
import com.neurocoevo.population._
import com.neurocoevo.speciation._
import com.neurocoevo.parameters._

import scala.util.Random

object RouletteWheel {

	/*
		select will take in the genomes based on a random number throw and using a cumulative sum
		of error will pick a genome. those with bigger
	*/
	def select(genomes:List[SpeciesMember], totalFitnessValue: Double): SpeciesMember = {
		
		val t = totalFitnessValue * Random.nextDouble
		selectAux(genomes, t, 0)

	}

	def selectAux(genomes:List[SpeciesMember], targetFitnessValue: Double, cumulativeError: Double): SpeciesMember = {
		try {
			val t = genomes.head.fitness
		}catch {
			case e: Exception => println(cumulativeError +", " + targetFitnessValue)
		}

		if(targetFitnessValue <= cumulativeError + genomes.head.fitness){
			genomes.head
		} else {
			selectAux(genomes.tail, targetFitnessValue, cumulativeError + genomes.head.fitness)
		}
	}



	def select(fns: List[((NetworkGenome, Int, MutationFunctionParameters) => Unit, Double)]): (NetworkGenome, Int, MutationFunctionParameters) => Unit = {
		val t = Random.nextDouble
		selectAux(fns, t, 0)

	}

	def selectAux(fns:List[((NetworkGenome, Int, MutationFunctionParameters) => Unit, Double)], target: Double, acc: Double): (NetworkGenome, Int, MutationFunctionParameters) => Unit = {
		if(target <= acc + fns.head._2){
			fns.head._1
		} else {
			selectAux(fns.tail, target, acc + fns.head._2)
		}
	}
}
