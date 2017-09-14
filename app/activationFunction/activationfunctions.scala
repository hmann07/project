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
    // def function(x: Double) = 1 / (1 + Math.exp(x * -1))
   
    var b = -4.9
   def function(x: Double) = 1 / (1 + Math.exp(x * b))
	 def derivative(x: Double) = this.function(x)  * (1 - this.function(x))
  }

 // as used in sharpNeat

  case class BipolarSigmoid() extends ActivationFunction {

   def function(x: Double) = (2.0 / (1.0 + Math.exp(-4.9 * x))) - 1.0
   def derivative(x: Double) = (-4.9 / 2) * (1 - Math.pow(this.function(x), 2))
  }

  case class HyperbolicTangent() extends ActivationFunction {

    val a =1.716
    val b = 0.667

   def function(x: Double) = ((2.0 * a) / (1.0 + Math.exp(-b * x))) - a
   def derivative(x: Double) = (2.0 * a * b * Math.exp(b * x))/Math.pow((1 + Math.exp(b * x)), 2)
// Found using https://www.wolframalpha.com/input/?i=((2.0+*+a)+%2F+(1.0+%2B+e%5E(-b+*+x)))+-+a
  }


  case class IdentityFunction() extends ActivationFunction {

	def function(x: Double) = x
	def derivative(x: Double) = 1
  }

  val neuronFunctions: List[String] = List("SIGMOID", "GAUSSIAN", "SINE", "TANH", "BIPOLARSIGMOID")
  def apply(fn: String): ActivationFunction = {
    fn match {
        case "GAUSSIAN" => Gaussian()
        case "SINE" => Sine()
        case "SIGMOID" => Sigmoid()
		    case "BIPOLARSIGMOID" => BipolarSigmoid()
        case "TANH" => HyperbolicTangent()
		    case "IDENTITY" => IdentityFunction()

    }
  }

}
