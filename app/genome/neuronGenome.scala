package com.neurocoevo.genome

import com.neurocoevo.neuron._
import com.neurocoevo.parameters.MutationFunctionParameters
import scala.util.Random

case class NeuronGenome(val innovationId: Int ,
                   val activationFunction: String,
                   val neuronType: String,
                   val bias: Int = 0,
                   val biasWeight: Double = ((Random.nextDouble * MutationFunctionParameters().connectionWeightRange) - (MutationFunctionParameters().connectionWeightRange /2)),
                   val layer: Double = 0,
				   val location: List[Double] = List.empty){

}
