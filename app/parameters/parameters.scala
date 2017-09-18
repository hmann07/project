package com.neurocoevo.parameters

import com.neurocoevo.evolution._

	case class EvaluatorParameters(
		//val evaluator: Evaluator =  VDEvaluator(75)
		val evaluator: Evaluator =  XOREvaluator(4)
		)

	case class SpeciationParameters(
		val c1: Double = 1.0,
		val c2: Double = 1.0,
		val c3: Double = 0.3,
		val speciationThreshold: Double = 6.0)


	case class PopulationParameters(
		val populationSize: Int = 150,
		val genomePath: String = ".\\resources\\annSubstrate.xml", // "cppnSubstrate.xml" or annSubstrate.xml or annBPSubstrate.xml or hyperneatAnnSubstrate or hyperneatAnnSubstrateVisualDiscrimination
		val agentType: String = "STD", // Options: HYPER, STD, BP
		val altGenomePath: String = ".\\resources\\hyperneatAnnSubstrateVisualDiscrimination.xml",
		val migrate: Boolean = false
	)

	case class BackPropParameters(
		val learningRate: Double = 0.1,
		val maxIterations: Int = 32000
	)

	case class OffspringParameters(
		val crossoverRate: Double = 0.20,
		val mutationRate: Double = 0.80,
		val elitismRate: Double = 0.2
	)


	case class MutationFunctionParameters (
		val offspringMutationRate: Double = 1,

	// Should add to 1.
		val perturbWeightRate: Double = 0.8,
		val addNeuronRate: Double = 0.1,
		val addConnectionRate: Double = 0.1,
	//
		val weightChangeProportion: Double = 0.8,
		val jiggleProportion: Double = 0.6, // non-jiggle are reset
		val connectionWeightRange: Double = 50.0,
		val biasWeightRange: Double = 50.0,
		val mutationPertubFactor: Double = 0.4,
		val recurrent: Boolean = false
	)


	case class OutputParameters (
		val outputPath: String = ".\\logs\\",
		val popOutputPath: String = ".\\logs\\"
		)

	case class UniverseParameters (
		val populationCount: Int = 50,
		val migrationRate: Double = 0.5)
