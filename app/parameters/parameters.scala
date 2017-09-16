package com.neurocoevo.parameters

import com.neurocoevo.evolution._

	case class EvaluatorParameters(
		val evaluator: Evaluator =  VDEvaluator(75)
		)

	case class SpeciationParameters(
		val c1: Double = 1.0,
		val c2: Double = 1.0,
		val c3: Double = 0.3,
		val speciationThreshold: Double = 3)


	case class PopulationParameters(
		val populationSize: Int = 75,
		val genomePath: String = ".\\resources\\cppnSubstrate.xml", // "cppnSubstrate.xml" or annSubstrate.xml or annBPSubstrate.xml or hyperneatAnnSubstrateVisualDiscrimination
		val agentType: String = "HYPER", // Options: HYPER, STD, BP
		val altGenomePath: String = ".\\resources\\hyperneatAnnSubstrateVisualDiscrimination.xml",
		val migrate: Boolean = false
	)

	case class BackPropParameters(
		val learningRate: Double = 0.1,
		val maxIterations: Int = 32000
	)

	case class OffspringParameters(
		val crossoverRate: Double = 0.25,
		val mutationRate: Double = 0.75,
		val elitismRate: Double = 0.01
	)


	case class MutationFunctionParameters (
		val offspringMutationRate: Double = 0.4,

	// Should add to 1.
		val perturbWeightRate: Double = 0.76,
		val addNeuronRate: Double = 0.04,
		val addConnectionRate: Double = 0.2,
	//
		val weightChangeProportion: Double = 0.9,
		val jiggleProportion: Double = 0.5, // non-jiggle are reset
		val connectionWeightRange: Double = 8.0,
		val mutationPertubFactor: Double = 0.15
	)


	case class OutputParameters (
		val outputPath: String = ".\\public\\logs\\",
		val popOutputPath: String = ".\\public\\logs\\"
		)

	case class UniverseParameters (
		val populationCount: Int = 1,
		val migrationRate: Double = 0.5)
