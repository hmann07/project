package com.neurocoevo.evolution

import scala.collection.immutable.SortedMap
import com.neurocoevo.network.Sensation

trait Evaluator(epochLength: Int){
	val aggregatedIterationValue: Double
	val fitness: Double
	val auxValue: Double
	def evaluateIteration(networkInput: List[Double], networkOutput: List[Double]): Evaluator
	def evaluateEpoch() : Evaluator
}

case class XOREvaluator(val epochLength: Int) extends Evaluator {
	
	override def evaluateIteration(networkInput: Sensation, networkOutput: SortedMap[Int, Double]): Evaluator = {
		  
  		  val error = networkInput.label(0) - networkOutput(3)
          val squaredError = math.pow(error, 2)
          
          self.copy(aggregatedIterationValue = squaredError)
	}


	override def evaluateEpoch() = {
		val evalfitness = 1 / aggregatedIterationValue
		self.copy(fitness = evalfitness, auxValue = aggregatedIterationValue)
	}
} 