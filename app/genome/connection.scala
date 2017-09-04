package com.neurocoevo.genome

import com.neurocoevo.substrate._
import scala.util.Random

/*
		Connection class holds source and destination connections. along with a randomised weight
		
*/

class Connection(val source: SubstrateNode, val destination: SubstrateNode) {
	val weight = (Random.nextDouble * 2) - 1
}