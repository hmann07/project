package com.neurocoevo.parameters

	case class SpeciationParameters(
		val c1: Double = 1.0,
		val c2: Double = 1.0,
		val c3: Double = 0.3,
		val speciationThreshold: Double = 2.5)


	case class PopulationParameters(
		val populationSize: Int = 150,
		val genomePath: String = ".\\resources\\annSubstrate.xml", // "cppnSubstrate.xml" or annSubstrate.xml
		val agentType: String = "STD", // Options: HYPER, STD, BP
		val altGenomePath: String = ".\\resources\\hyperneatAnnSubstrate.xml"
	)

	case class BackPropParameters(
		val learningRate: Double = 0.1
	)

	case class OffspringParameters(
		val crossoverRate: Double = 0.25,
		val mutationRate: Double = 0.75,
		val elitismRate: Double = 0.1
	)

	// Should add to 1.
	case class MutationFunctionParameters (
		val perturbWeightRate: Double = 0.8,
		val addNeuronRate: Double = 0.1,
		val addConnectionRate: Double = 0.1
	)

	case class OutputParameters (
		val outputPath: String = ".\\public\\logs\\",
		val popOutputPath: String = ".\\public\\logs\\"
		)

	case class UniverseParameters (
		val populationCount: Int = 50)
