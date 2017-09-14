package com.neurocoevo.parameters

import com.neurocoevo.evolution

	case class Evaluator(
		val evaluator: new XOREvaluator(4)
		)

	case class SpeciationParameters(
		val c1: Double = 1.0,
		val c2: Double = 1.0,
		val c3: Double = 0.3,
		val speciationThreshold: Double = 2.1)


	case class PopulationParameters(
		val populationSize: Int = 150,
		val genomePath: String = ".\\resources\\annSubstrate.xml", // "cppnSubstrate.xml" or annSubstrate.xml or annBPSubstrate.xml
		val agentType: String = "STD", // Options: HYPER, STD, BP
		val altGenomePath: String = ".\\resources\\hyperneatAnnSubstrate.xml"
	)

	case class BackPropParameters(
		val learningRate: Double = 0.1,
		val maxIterations: Int = 32000
	)

	case class OffspringParameters(
		val crossoverRate: Double = 0.10,
		val mutationRate: Double = 0.90,
		val elitismRate: Double = 0.02
	)


	case class MutationFunctionParameters (
		val offspringMutationRate: Double = 0.5,

	// Should add to 1.
		val perturbWeightRate: Double = 0.98,
		val addNeuronRate: Double = 0.01,
		val addConnectionRate: Double = 0.01,
	//
		val weightChangeProportion: Double = 0.6,
		val jiggleProportion: Double = 0.8, // non-jiggle are reset
		val connectionWeightRange: Double = 8.0,
		val mutationPertubFactor: Double = 0.4
	)


	case class OutputParameters (
		val outputPath: String = ".\\public\\logs\\",
		val popOutputPath: String = ".\\public\\logs\\"
		)

	case class UniverseParameters (
		val populationCount: Int = 50)
