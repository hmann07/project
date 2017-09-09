package com.neurocoevo.genome

import com.neurocoevo.neuron._
import scala.util.Random

case class NeuronGenome(val innovationId: Int ,
                   val activationFunction: String,
                   val neuronType: String,
                   val bias: Int = 0,
                   val biasWeight: Double = (Random.nextDouble * 2) - 1,
                   val layer: Double = 0,
				   val location: List[Double] = List.empty){

}
