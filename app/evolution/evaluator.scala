package com.neurocoevo.evolution

import scala.collection.immutable.SortedMap
import com.neurocoevo.network.Network.Sensation


trait Evaluator {
	val epochLength: Int
	val fitness: Double
	val auxValue: Double
	def evaluateIteration(networkInput: Sensation, networkOutput: SortedMap[Int, Double]): Evaluator
	def evaluateEpoch() : Evaluator
	
}

case class XOREvaluator(
	val epochLength: Int,
	val aggregatedIterationValue: Double = 0,
	val fitness: Double = 0,
	val auxValue: Double = 0) extends Evaluator {
	
	override def evaluateIteration(networkInput: Sensation, networkOutput: SortedMap[Int, Double]): Evaluator = {
		  
  		  val error = networkInput.label(0) - networkOutput(3)
  		  val squaredError = math.pow(error, 2)
          
          this.copy(aggregatedIterationValue = aggregatedIterationValue + squaredError)
	}


	override def evaluateEpoch() = {
		
		val evalfitness = 1 / aggregatedIterationValue
		
		this.copy(fitness = evalfitness, auxValue = aggregatedIterationValue)
	}


} 


