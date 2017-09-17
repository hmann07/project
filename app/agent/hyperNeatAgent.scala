package com.neurocoevo.agent

import com.neurocoevo.genome._
import com.neurocoevo.neuron._
import com.neurocoevo.substrate.SubstrateNode
import com.neurocoevo.network._
import com.neurocoevo.experience._
import com.neurocoevo.population._
import com.neurocoevo.innovation._
import com.neurocoevo.speciation.SpeciesMember
import com.neurocoevo.parameters.MutationFunctionParameters
import com.neurocoevo.evolution.RouletteWheel

import scala.util.Random
import scala.collection.immutable.HashMap

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}

object HyperNeatAgent {

	def props(cppnGenome: NetworkGenome, annSubstratePath: String, experience: ActorRef, innovationAgent: ActorRef, species: Int = 0): Props = Props(new HyperNeatAgent(cppnGenome, annSubstratePath, experience, species, innovationAgent))

    case class NewChild(genome: NetworkGenome, name: Int)

	case class AgentSettings(cppn: ActorRef, ann: ActorRef = null, annGenome: NetworkGenome = null)

	case class ConfiguredNetwork(networkGenome: NetworkGenome)
}


/* HyperNeatAgent.
		HyperNeatAgent will have a genome that defines a CPPN.
		using the Genome it must set up the actors for the network set up the connections and


*/

class HyperNeatAgent(cppnGenome: NetworkGenome, annSubstratePath: String, experience: ActorRef, species: Int, innovationAgent: ActorRef) extends Actor with ActorLogging {
	import context._
    import HyperNeatAgent._


    override def postStop() {

        experience ! "STOP"
        context.children.foreach(c=> context.stop(c))

    }

	// we will fire up a Network for the CPPN. We will have to wait for the network to confirm it is ready.

	val cppn = actorOf(Network.props(cppnGenome), "cppn")

	def receive = runningAgent(AgentSettings(cppn = cppn))

	def runningAgent(s: AgentSettings): Receive = {

    	case Network.NetworkReady(networkGenome) =>


			if(sender().path.name == "cppn"){

				// then we should now create the ANN based on the output of the CPPN.

				val annFactory = actorOf(ActorGenomeFactory.props(annSubstratePath), "annFac")


			} else {

				// we assume that this must have been the ANN. so we can send it its inputs we can close the ANN factory since we no longer need it.
				
				context.actorSelection("annFac") ! akka.actor.PoisonPill

				// all set up, start getting signals to process.

				experience ! "perceive"

			}

		// this comes from the actor based genome factory
		case ConfiguredNetwork(annNetwork) =>

			val annActor = actorOf(Network.props(annNetwork), "ann")
			context become runningAgent(s.copy(ann = annActor, annGenome = annNetwork))

    	case "propagated" =>

    		// finished learning form that sensation... give me another.
    		experience ! "perceive"


    	case Experience.Event(e, l) =>

			val t = Network.Sensation(1, e, l, "EVOLVE")
			//println(t)
    		s.ann ! t


        // Received when a network has completed one signal of a pattern and needs another
        case "newSignal" =>
            //println("newsg request")
            // finished processing sensation... give me another.
            experience ! "perceive"


        //  Received when a network has processed all expected patterns from a test set.
        case Network.Matured(g, fitnessValue, sse) =>

			//println("network matured")
			val ts = System.currentTimeMillis()
			// tell population, send the CPPN which will be subject to genetic operators.
			// used the agent version of maturerd so that population can match..

            parent ! Agent.Matured(cppnGenome, fitnessValue, sse, species, ts, s.annGenome)

        // receieved some instructions for crossing over two genomes..

		case Population.Elite(genome, genomeNumber) =>


			parent ! Agent.NewChild(genome.copy(id = genomeNumber), genomeNumber)

		case Population.Mutate(genome, genomeNumber) =>

				mutate(genome, genomeNumber)

		case Population.Crossover(g, genomeNumber) =>
			val crossedGenome = crossover(g, genomeNumber)
			if(crossedGenome.id != -1){
				// we are not mutating so can forward onwards
				parent ! Agent.NewChild(crossedGenome, genomeNumber)
			}



  		case "ProcessRequest" =>
  			sender() ! "Ready"

  	}


    // MUTATIONS
    // Genrally it seems mutation does not get applied until cross over has happened...
		// Even further implemenations of Hyper neat appear to not mutate when corssover has happended...

		// a general function that will return a function that will mutate a genome based on a random probability.

		def mutate(genome: NetworkGenome, genomeNumber: Int) = {

			val params = MutationFunctionParameters()

			val mutationFunctions = List(
					(mutatePerturbWeight(_, _, _),params.perturbWeightRate ),
					(mutateAddNeuron(_, _, _), params.addNeuronRate),
					(mutateAddConnection(_, _, _), params.addConnectionRate)
					)

			val mutationFunction = RouletteWheel.select(mutationFunctions)


			mutationFunction(genome, genomeNumber, params)

		}


    // Perturb weights
        // find a connection
        // vary its weight slightly. or a lot. or by something....

			def mutatePerturbWeight(genome: NetworkGenome, genomeNumber: Int, params: MutationFunctionParameters) = {


					// Currently go 50.50 on whether or not to change bias or connection weight...
					// currently separate since I am keeping Bias inside the neuron rather than the connection.

					// should this be parameterised?...

					// Going to consider mutating bias all the time..

								// change connection weight
								// HyperNeat Implementation doesn't just change one it changes multiple.The number of changes is based on a random number or a fixed quantity,
								// whilst always making sure at least one is changes.

								val weightRangeCap = params.connectionWeightRange / 2

								val connections = genome.connections.values
								//val totalConnections = connections.length
								//val connectionToChange = connections(Random.nextInt(totalConnections))

								val newConnections: HashMap[Int, ConnectionGenome] = genome.connections ++ connections.foldLeft(HashMap[Int, ConnectionGenome]()) {(conns, connection) =>

										conns + (connection.innovationId -> {

											// a chance that this connection will not be changed at all
											if(Random.nextDouble < params.weightChangeProportion) {

												// a chance that the weight will be completely reset or just changed slightly.
												if(Random.nextDouble < 0.9) {
												// Could insert some sort of factor here to control how much it changes. also in sharpNeat and Erlang there is a weight cap

													// sharpNEAT restrict to small weights between 1...
													val dW = {connection.weight + (((Random.nextDouble * params.connectionWeightRange) - weightRangeCap) * params.mutationPertubFactor)} match {

														case e if e > weightRangeCap => weightRangeCap
														case e if e < -weightRangeCap => -weightRangeCap
														case e => e
													}

													val c = connection.copy(weight = dW)
													c
													} else {

														val c = connection.copy(weight = (Random.nextDouble * params.connectionWeightRange) - weightRangeCap)
														c
													}

											} else {
												connection
											}
										})
								}

								//parent ! NewChild(new NetworkGenome(genomeNumber, genome.neurons, newConnections), genomeNumber)

								// Now change bias weight

								val neurons = genome.neurons.values.toList
								val totalNeurons = neurons.length
								val neuronToChange = neurons(Random.nextInt(totalNeurons))

								// Could insert some sort of factor here to control how much it changes. also in sharpNeat and Erlang there is a weight cap

								val newNeurons: HashMap[Int, NeuronGenome] = genome.neurons ++ neurons.foldLeft(HashMap[Int, NeuronGenome]()){(acc, currentNeuron) =>
									acc + (currentNeuron.innovationId -> {
									// do we change the bias?
										if(Random.nextDouble < params.weightChangeProportion) {

											//do we purturb or reset?

											if(Random.nextDouble < params.jiggleProportion) {
												// perturb
												val dW = {neuronToChange.biasWeight + (((Random.nextDouble * params.connectionWeightRange) - weightRangeCap) * params.mutationPertubFactor)} match {

													case e if e > weightRangeCap => weightRangeCap
													case e if e < -weightRangeCap => -weightRangeCap
													case e => e
												}

												currentNeuron.copy(biasWeight = dW)
											} else {
												// reset
												currentNeuron.copy(biasWeight = ((Random.nextDouble * params.connectionWeightRange) - weightRangeCap))
											}
										} else {
											// no change to bias.
											currentNeuron
										}
									})
								}
								//println("mutate weight")
								parent ! Agent.NewChild(new NetworkGenome(genomeNumber, newNeurons, newConnections), genomeNumber)

						//}
		}

    // Add connection
        // <decscription> find two nodes without a connection
        // add the connection.
        // <param> genome -> the genome due to be mutated.
        // <return> genome -> A new copy of the genome with a new connection added.


    // add node Or splice
    // find connection between two nodes. deactive connection, replace with two new connections and a node


    // add node
    // pick two random nodes. and add a new connected node.

    def mutateAddConnection (genome: NetworkGenome, genomeNumber: Int, params: MutationFunctionParameters) = {



        // first take all the keys / innovation ids for the neurons
        		val all = (genome.inputNodes ++ genome.hiddenNodes ++ genome.outputNodes)
        		val dest = (genome.hiddenNodes ++ genome.outputNodes)
				val validSrcNeurons = genome.neurons.keys.toList
				val validDestNeurons = dest.keys.toList // Mustn't connect to the input.

				// select two random neurons
				val n1 = validSrcNeurons(Random.nextInt(validSrcNeurons.length))
				val n2 = validDestNeurons(Random.nextInt(validDestNeurons.length))

				// check if recurrent // Currently defaulting to stop recurrent CPPN connections. FOR VD experiment only.
				if(!params.recurrent &&  all(n1).layer >= dest(n2).layer) {
					// we are not allowing recurrent but proposed connection is...
					// try a differnet mutation...
					context.self ! Population.Mutate(genome, genomeNumber)

					} else {

						// Go ahead and create it.
						// check not already connected locally

						val existingEntry = genome.connections.values.find(te => te.from == n1 && te.to == n2)

						existingEntry match {

							case Some(e) => {
								// these two are already connected so just return the genome.
								// TODO: We can probably have a few goes at this.. say try 4 times if no success then give up.

								// try a differnet mutation...
								context.self ! Population.Mutate(genome, genomeNumber)

							}

							case None => {
								// the connection does not exist already, in this network, check innovation number in case elsewhere.
								innovationAgent  ! Innovation.NewConnectionProposal(n1, n2)
				        		context become mutatingGenomeAddConnection(genome, genomeNumber, params)
							}
						}
					}
    }

    /* <Description> mutateAddNeuron: As generally described by the hyperneat papers pick a connection, disable it
                     create two new connections and a new node inbetween.
     <param> NetworkGenome: the network genome to be mutated
     <return> NetworkGenome: A genome with a new neuron added.
    */

    def mutateAddNeuron (genome: NetworkGenome, genomeNumber: Int, params: MutationFunctionParameters) = {

        // First, pick a connection from the genome to split.. Randomly...

		// Avoid splitting a recurrent connection.. for now... In theory it creates unnecessary structure..
		// though perhaps fine tunes the signal and seems at face value to converge quicker. it's behaviour is a little difficult 
		// to reason about in terms of recurrency in this model. It's a bit like a delay.
		// The signal would take multiple iterations to get to a feedforward node. 

        val connIds: List[Int] = genome.connections.filter(x=> !x._2.recurrent).keys.toList
		
        val connToReplace: Int = connIds(Random.nextInt(connIds.length))
        val connectionToSplit: ConnectionGenome = genome.connections(connToReplace)

        //println("split conn innov id: " + connectionToSplit.innovationId + ", which breaks " + connectionToSplit.from + " and " + connectionToSplit.to)
        // Ask the innovation Actor if anyone has already split this connection. If yes, we should use
        // the same innovation id of both neuron and the two connections

        innovationAgent  ! Innovation.NewNeuronProposal(connectionToSplit.from, connectionToSplit.to)
        context become mutatingGenome(genome, connectionToSplit.innovationId, genomeNumber, params)
    }


    def mutatingGenome(genome: NetworkGenome, oldConnection: Int, genomeNumber: Int, params: MutationFunctionParameters): Receive = {

        case Innovation.NewNeuronConfirmation(neuronData) =>
            // using the neuron data change the NetworkGenome
            //println("got some innovation id data... lets mutate that neuron...")
            //println("mutate")
            val oldConnectionGenome: ConnectionGenome = genome.connections(oldConnection)

            val newCons = genome.connections +
                    (oldConnection -> oldConnectionGenome.copy(enabled = false),
                    	// NEAT paper indicates that connection into new get weight = 1, conn out gets old weight
                     neuronData.connection1 -> new ConnectionGenome(neuronData.connection1, neuronData.fromNeuron, neuronData.newNeuron, 1 ),   //new ConnectionGenomes
                     neuronData.connection2 -> new ConnectionGenome(neuronData.connection2, neuronData.newNeuron, neuronData.toNeuron, oldConnectionGenome.weight))  //new ConnectionGenomes

            val activationFns = List("SIGMOID", "GAUSSIAN", "SINE", "TANH", "BIPOLARSIGMOID")
			val activationFn = activationFns(Random.nextInt(activationFns.length))

            val newNeurons = genome.neurons + (neuronData.newNeuron-> new NeuronGenome(
                    neuronData.newNeuron,
                    activationFn, // In case of CPPN Needs to be randomly selected
                    "hidden",  // Assume we can't ad or remove inputs or outputs.
                    -1, // Bias val
                    ((Random.nextDouble * params.biasWeightRange) - (params.biasWeightRange /2)), // Bias weight
                    (genome.neurons(oldConnectionGenome.from).layer + genome.neurons(oldConnectionGenome.to).layer) / 2 // Layer. SHould be the sum of the layers of the 2 neurons previously conected / 2
                    ))

            //println("mutate add neuron")
            context.parent ! Agent.NewChild(new NetworkGenome(genomeNumber, newNeurons, newCons), genomeNumber)
            //println(newCons.toString)

    }

		def mutatingGenomeAddConnection(genome: NetworkGenome, genomeNumber: Int, params: MutationFunctionParameters): Receive = {

				case Innovation.NewConnectionConfirmation(connectionData) =>

					//if({ genome.neurons(connectionData.fromNeuron).layer >= genome.neurons(connectionData.toNeuron).layer })
					//	println("creating a recurrent connection")

					val newConnections = genome.connections + (connectionData.innovationId ->
						new ConnectionGenome(
						connectionData.innovationId,
						connectionData.fromNeuron,  //from
						connectionData.toNeuron,  // to
						((Random.nextDouble * params.connectionWeightRange) - (params.connectionWeightRange /2)), //weight
						true, // enabled
						{ genome.neurons(connectionData.fromNeuron).layer >= genome.neurons(connectionData.toNeuron).layer })) // recurrent
				//println("mutate add conn")
	 			context.parent ! Agent.NewChild(new NetworkGenome(genomeNumber, genome.neurons, newConnections), genomeNumber)

		}

	// CROSSOVER
	// This function will be passed to the agents or the stronger of the agents.
	// benefit being it would get done in parallel in large populations this matters.
	// crossover in NEAT actually refers specifically to the connections. Though the Neruons are obviously involved...
	// the neurons for the new genome can be retrieved by a pass through the new connection genome.
	// but which do we take?? In general the Neruons wiht the same innovation number should both be the same
	// however I have added Bias to the neuron itself meaning during learning/mutation they will diverge. Equally perhaps i tis interesting
	// to include mutation of the activation function. Could take neuron
	// randomly or that of the fittest.


	def crossover(g: List[SpeciesMember], genomeNumber: Int): NetworkGenome = {

		g.length match {
			case 2 => {
				val performanceSortedGenomes = g.sortBy(- _.fitness)
				val networkGenome1 = performanceSortedGenomes(0).genome // Due to the previous sort this will always be the strongest.
				val networkGenome2 = performanceSortedGenomes(1).genome

				val crossedConnections: HashMap[Int, ConnectionGenome] = networkGenome1.connections.foldLeft(HashMap[Int, ConnectionGenome]()) { (m, c) =>

					// First check if genome2 has this connection
					val matching = networkGenome2.connections.contains(c._1)



					if (matching) {
						// Randomly take one or other of the genomes.
						val matched = networkGenome2.connections(c._1)
						m + (List(c, (matched.innovationId -> matched))(Random.nextInt(2)))
					} else {
						// This is the stronger genome (by virtue of being genome1) take its additional parts. discard the other.
						// TODO: Do we not even want to consider the weaker disjoints or excesss genes. in sharpNeat this appears to be toggled
						// at one point (though commented out) even randomly..
						m + c
					}
					}

					// now go through the connections and make sure we have neurons for both ends
				val newGenomes = crossedConnections.foldLeft(HashMap[Int, NeuronGenome]()) { (m, n) =>
					try{
						m + (n._2.from -> networkGenome1.neurons(n._2.from), n._2.to -> networkGenome1.neurons(n._2.to) )
					} catch {
						case e: Exception => println(networkGenome1 + " \n" + networkGenome2)
					}

					m + (n._2.from -> networkGenome1.neurons(n._2.from), n._2.to -> networkGenome1.neurons(n._2.to) )

				  }
				//println(crossedConnections)
				//println(newGenomes)

				if(Random.nextDouble <= MutationFunctionParameters().offspringMutationRate){
					// then we also mutate the child...
					context.self ! Population.Mutate(new NetworkGenome(genomeNumber, networkGenome1.neurons, crossedConnections), genomeNumber)

					// Using -1 to tell the agent that we are going to do some extra processing...
					new NetworkGenome(-1, networkGenome1.neurons, crossedConnections)

				} else {
					// otherwise send it as is
					new NetworkGenome(genomeNumber, networkGenome1.neurons, crossedConnections)
				}
			}

			case _ => {
			//  NEAT software shows if only one parent we mutate it.
			context.self ! Population.Mutate(new NetworkGenome(genomeNumber, g(0).genome.neurons, g(0).genome.connections), genomeNumber)

			// Using -1 to tell the agent that we are going to do some extra processing...

			new NetworkGenome(-1, g(0).genome.neurons, g(0).genome.connections)
			}
		}
	}
}
