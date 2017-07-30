package com.neurocoevo.genome

import scala.collection.immutable.HashMap
import scala.util.Random
import com.neurocoevo.speciation.SpeciationParameters

case class NetworkGenome(val neurons: HashMap[Int, NeuronGenome],
					val connections1: HashMap[Int, ConnectionGenome]){

	///a little bit bad design here. Naming could be imporved.
	// and also NEAT paper suggests that disabled genes could be re-enabled.
	val connections = connections1.filter(x=> x._2.enabled)
	val inputNodes: HashMap[Int, NeuronGenome] = neurons.filter(n => n._2.neuronType == "input")
	val outputNodes: HashMap[Int, NeuronGenome] = neurons.filter(n => n._2.neuronType == "output")
	val hiddenNodes: HashMap[Int, NeuronGenome] = neurons.filter(n => n._2.neuronType == "hidden")


	def compareTo(genome: NetworkGenome, params: SpeciationParameters): Double = {


		val g1 = connections.keySet
		val g2 = genome.connections.keySet

		// these maxes might be more efficient if captured as the genomes mutate...
		val g1max = g1.max
		val g2max = g2.max

		val biggestSize = {if (g1.size > g2.size) g1.size else g2.size}

		// excess genes by definition will only appear in one of the genomes.

		val excessGenes: Set[Int] = {

			if(g1max < g2max) {
				// g2 has at least 1 excess gene.
				// collect all those in g2 that are greater than the biggest innovation in g1
				g2.filter(x => x > g1max)
			} else if(g1max > g2max) {
				// g1 has at least 1 excess gene
				g1.filter(x => x > g2max)
			} else {
				Set.empty
			}
		}

		// disjoint genes are those that appear in one or the other but not both. Xor essentially. or the compliment of the union with respect to the intersection

		val intersection = (g1 & g2)

		val disjointGenes = (g1 | g2).diff(intersection)

		val matchedGenes: Double = intersection.foldLeft(0.0)((r,c) => r + math.abs(connections(c).weight + genome.connections(c).weight).toDouble ) / intersection.size

		((excessGenes.size / biggestSize) * params.c1) + ((disjointGenes.size / biggestSize) * params.c2) + (matchedGenes * params.c3)

	}


}
