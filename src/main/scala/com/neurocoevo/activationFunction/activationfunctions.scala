package com.neurocoevo.activationfunction

abstract class ActivationFunction {
	
	def function(x: Double): Double

}

abstract class DifferentiableFunction extends ActivationFunction {

	 def function(x: Double): Double 
	
	 def derivative(x: Double): Double 

}


class Bias extends ActivationFunction {

	def function(x: Double) = 1

}
	
class gaussian extends ActivationFunction{

	def function(x: Double) = 2 * Math.exp(-Math.pow(x * 2.5, 2)) - 1

} 

class sine extends ActivationFunction{

	def function(x: Double) = Math.sin(x*2)

} 

class Sigmoid extends DifferentiableFunction {
	
	def function(x: Double) = 1 / (1 + Math.pow(Math.E, -x))

	def derivative(x: Double) = function(x)  * (1 -  (1 / (1 + Math.pow(Math.E, -x))))
}


class Identity extends DifferentiableFunction {
	
	def function(x: Double) = 1

	def derivative(x: Double) = 1
}