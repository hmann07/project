package com.neurocoevo.genome

import com.neurocoevo.neuron._

class NeuronGenome(val innovationId: Int , 
                   val activationFunction: String, 
                   val neuronType: String, 
                   val bias: Int = 0, 
                   val biasWeight: Double,
                   val layer: Double = 0){

}
