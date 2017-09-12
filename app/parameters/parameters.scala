package com.neurocoevo.parameters

	case class SpeciationParameters(
		val c1: Double = 1.0,
		val c2: Double = 1.0,
		val c3: Double = 0.3,
		val speciationThreshold: Double = 2.0)


	case class PopulationParameters(
		val populationSize: Int = 100,
		val genomePath: String = ".\\resources\\cppnSubstrate.xml", // "cppnSubstrate.xml" or annSubstrate.xml or annBPSubstrate.xml
		val agentType: String = "HYPER", // Options: HYPER, STD, BP
		val altGenomePath: String = ".\\resources\\hyperneatAnnSubstrate.xml"
	)

	case class BackPropParameters(
		val learningRate: Double = 0.1,
		val maxIterations: Int = 32000
	)

	case class OffspringParameters(
		val crossoverRate: Double = 0.25,
		val mutationRate: Double = 0.75,
		val elitismRate: Double = 0.1
	)


	case class MutationFunctionParameters (
	// Should add to 1.
		val perturbWeightRate: Double = 0.8,
		val addNeuronRate: Double = 0.1,
		val addConnectionRate: Double = 0.1,
	//
		val weightChangeProportion: Double = 0.6,
		val jiggleProportion: Double = 0.7, // non-jiggle are reset
		val connectionWeightRange: Double = 8.0,
		val mutationPertubFactor: Double = 0.05
	)


	case class OutputParameters (
		val outputPath: String = ".\\public\\logs\\",
		val popOutputPath: String = ".\\public\\logs\\"
		)

	case class UniverseParameters (
		val populationCount: Int = 1)
