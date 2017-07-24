package com.neurocoevo.genome

import scala.util.Random

case class ConnectionGenome(
	val innovationId: Int,
	val from: Int,
	val to: Int,
	val weight: Double = Random.nextDouble(),
	val enabled: Boolean = true,
	val recurrent: Boolean = false)
