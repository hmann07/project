package com.neurocoevo.activationfunction



trait ActivationFunction{
  def function(x:Double): Double
  def derivative(x:Double): Double
}


object ActivationFunction {

  case class Gaussian() extends ActivationFunction {

	def function(x: Double) = 2 * Math.exp(-Math.pow(x * 2.5, 2)) - 1
	def derivative(X: Double) = 0.0
  } 

  case class Sine() extends ActivationFunction {

	def function(x: Double) = Math.sin(x*2)
	def derivative(X: Double) = 0.0
  } 

  case class Sigmoid() extends ActivationFunction {
	
    def function(x: Double) = 1 / (1 + Math.exp(x * -4.9))
	def derivative(x: Double) = this.function(x)  * (1 - this.function(x))
  }


  case class InputFunction() extends ActivationFunction {
	
	def function(x: Double) = 1
	def derivative(x: Double) = 1
  } 

  
  def apply(fn: String): ActivationFunction = {
    fn match {
        case "GAUSSIAN" => Gaussian()
        case "SINE" => Sine()
        case "SIGMOID" => Sigmoid()
        case "INPUTFUNCTION" => InputFunction()
    }
  }

}




