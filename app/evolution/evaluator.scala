package com.neurocoevo.evolution

trait Evaluator(epochLength: Int){
	val aggregatedIterationValue: Double
	val fitnessValue: Double
	def evaluateIteration(networkInput: List[Double], networkOutput: List[Double]): Evaluator
	def evaluateEpoch() : Evaluator
} 