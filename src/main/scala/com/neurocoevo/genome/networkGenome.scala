package com.neurocoevo.genome

import scala.collection.immutable.HashMap
import scala.util.Random
import com.neurocoevo.speciation.SpeciationParameters

case class NetworkGenome(val neurons: HashMap[Int, NeuronGenome],
					val connections1: HashMap[Int, ConnectionGenome]){

	///a little bit bad design here. Naming could be imporved.
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

		val disjointGenes = (g1 | g2).diff((g1 & g2))

		(excessGenes.size * params.c1) + (disjointGenes.size * params.c2) * params.c3

	}
}
