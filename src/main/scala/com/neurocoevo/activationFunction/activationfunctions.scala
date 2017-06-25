package com.neurocoevo.activationfunctions

object ActivationFunctions {
	
	val bias = (x: Double) => 1
	
	val gaussian = (x: Double) => 2 * Math.exp(-Math.pow(x * 2.5, 2)) - 1

	val sine = (x: Double) => Math.sin(x*2)

	val sigmoid = (x: Double) => 1 / (1 + Math.pow(Math.E, -x))

}