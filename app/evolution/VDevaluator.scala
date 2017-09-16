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
  		  
  		  // First work out the output that gave the max. -122 becuase all outputs start from 122 and based from 0 (Defined by  training data - nodes start from 1.)
  		  val maxNeuron = networkOutput.maxBy(m => m._2)._1 - 122
		 
		
  		  val x2 = (maxNeuron) % 11
		  val y2 = Math.floor((maxNeuron) / 11)

  		  // Then work out the distance of that output from the true output
          
  		  val d = Math.pow(x1 - x2,2) + Math.pow(y1 - y2, 2)
  		  val actuald = math.sqrt(d)
  		  //println(d + ", " + networkInput.label + ", " + networkOutput.maxBy(m => m._2) )
          this.copy(aggregatedIterationValue = aggregatedIterationValue + d, auxValue = auxValue + actuald )
	}


	override def evaluateEpoch() = {
		
		  // we need to inverse distnace since we want the smaller the distance the  better fitness

		  var invD = 1 / ((aggregatedIterationValue) / 75)
			
			// Aux value is the average distance... 

		this.copy(fitness = invD, auxValue = auxValue / 75)
	}
} 