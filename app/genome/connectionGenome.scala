package com.neurocoevo.genome

import scala.util.Random
import com.neurocoevo.parameters.MutationFunctionParameters

case class ConnectionGenome(
	val innovationId: Int,
	val from: Int,
	val to: Int,
	val weight: Double = ((Random.nextDouble * MutationFunctionParameters().connectionWeightRange) - (MutationFunctionParameters().connectionWeightRange /2)),
	val enabled: Boolean = true,
	val recurrent: Boolean = false)


	object ConnectionGenome {

		implicit def orderingByInnovation[A <: ConnectionGenome]: Ordering[A] = Ordering.by(e => e.innovationId)

	}
