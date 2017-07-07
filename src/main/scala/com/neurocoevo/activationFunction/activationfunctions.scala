package com.neurocoevo.activationfunction



abstract class ActivationFunction {
	
	def function(x: Double): Double
	def derivative(x: Double): Double
}




class GAUSSIAN extends ActivationFunction{

	def function(x: Double) = 2 * Math.exp(-Math.pow(x * 2.5, 2)) - 1
	def derivative(X: Double) = 0.0
} 

class SINE extends ActivationFunction{

	def function(x: Double) = Math.sin(x*2)
	def derivative(X: Double) = 0.0
} 

class SIGMOID extends ActivationFunction {
	
	def function(x: Double) = 1 / (1 + Math.exp(x * -1))

	def derivative(x: Double) = this.function(x)  * (1 - this.function(x))
}


class INPUTFUNCTION extends ActivationFunction {
	
	def function(x: Double) = 1

	def derivative(x: Double) = 1
}
