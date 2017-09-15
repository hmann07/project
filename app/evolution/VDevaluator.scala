package com.neurocoevo.evolution

import scala.collection.immutable.SortedMap
import com.neurocoevo.network.Network.Sensation


case class VDEvaluator(
	val epochLength: Int,
	val aggregatedIterationValue: Double = 0,
	val fitness: Double = 0,
	val auxValue: Double = 0) extends Evaluator {
	
	override def evaluateIteration(networkInput: Sensation, networkOutput: SortedMap[Int, Double]): Evaluator = {
		  

		  val x1 = networkInput.label(0)
		  val y1 = networkInput.label(1)
  		  
  		  // First workout the output that gave the max.
  		  val maxNeuron = networkOutput.maxBy(m => m._2) 
		 
  		  val x2 = maxNeuron._1 - 1 % 11
		  val y2 = math.floor(maxNeuron._1 - 1 / 11)

  		  // Then work out the distance of that output from the true output
          
  		  val d = Math.sqrt(Math.pow(x1 - x2,2) + Math.pow(y1 - y2, 2))

          this.copy(aggregatedIterationValue = aggregatedIterationValue + d)
	}


	override def evaluateEpoch() = {
		
		  // we need to inverse distnace since the smaller the better

		  var invD = 1 / aggregatedIterationValue
		
			// Aux value is the average distance... 

		this.copy(fitness = invD, auxValue = aggregatedIterationValue / 75)
	}
} 