package com.neurocoevo.agent

import com.neurocoevo.genome._
import com.neurocoevo.neuron._
import com.neurocoevo.substrate.SubstrateNode
import com.neurocoevo.network._
import com.neurocoevo.experience._
import com.neurocoevo.population._
import com.neurocoevo.innovation._
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

			// tell population, send the CPPN which will be subject to genetic operators.
			// used the agent version of maturerd so that population can match..

            parent ! Agent.Matured(cppnGenome, fitnessValue, sse, species, s.annGenome)

        // receieved some instructions for crossing over two genomes..

		case Population.Elite(genome, genomeNumber) =>


			parent ! Agent.NewChild(genome.copy(id = genomeNumber), genomeNumber)

		case Population.Mutate(genome, genomeNumber) =>

				mutate(genome, genomeNumber)

		case Population.Crossover(g, f, genomeNumber) =>

  			parent ! Agent.NewChild(f(g, genomeNumber), genomeNumber)

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
				(mutatePerturbWeight(_, _),params.perturbWeightRate ),
				(mutateAddNeuron(_, _), params.addNeuronRate),
				(mutateAddConnection(_, _), params.addConnectionRate))

			val mutationFunction = RouletteWheel.select(mutationFunctions)


			mutationFunction(genome, genomeNumber)

		}


    // Perturb weights
        // find a connection
        // vary its weight slightly. or a lot. or by something....

			def mutatePerturbWeight(genome: NetworkGenome, genomeNumber: Int) = {


					// Currently go 50.50 on whether or not to change bias or connection weight...
					// currently separate since I am keeping Bias inside the neuron rather than the connection.

					// should this be parameterised?...

					if(Random.nextDouble < Random.nextDouble) {

									// change connection weight
									// HyperNeat Implementation doesn't just change one it changes multiple.The number of changes is based on a random number or a fixed quantity,
									// whilst always making sure at least one is changes.

									val connections = genome.connections.values
									//val totalConnections = connections.length
									//val connectionToChange = connections(Random.nextInt(totalConnections))

									val newConnections: HashMap[Int, ConnectionGenome] = genome.connections ++ connections.foldLeft(HashMap[Int, ConnectionGenome]()) {(conns, connection) =>

											conns + (connection.innovationId -> {

												// a chance that this connection will not be changed at all
												if(Random.nextDouble < 0.9) {


																// a chance that the weight will be completely reset or just changed slightly.
																if(Random.nextDouble < 0.9) {
																// Could insert some sort of factor here to control how much it changes. also in sharpNeat and Erlang there is a weight cap
																	val c = connection.copy(weight = connection.weight + ((Random.nextDouble * 4) - 2) * Random.nextDouble)
																	c
																} else {
																	val c = connection.copy(weight = (Random.nextDouble * 2) - 1)
																	c
																}


														} else {
															connection
														}
												})
									}

									parent ! Agent.NewChild(new NetworkGenome(genomeNumber, genome.neurons, newConnections), genomeNumber)

							} else {
									// change bias weight
									val neurons = genome.neurons.values.toList
									val totalNeurons = neurons.length
									val neuronToChange = neurons(Random.nextInt(totalNeurons))

									// Could insert some sort of factor here to control how much it changes. also in sharpNeat and Erlang there is a weight cap
									//val perturbedNeuron = neuronToChange.copy(biasWeight = neuronToChange.biasWeight + ((Random.nextDouble * 8) - 4) * Random.nextDouble)

									val newNeurons: HashMap[Int, NeuronGenome] = genome.neurons ++ neurons.foldLeft(HashMap[Int, NeuronGenome]()){(acc, currentNeuron) =>
										acc + (currentNeuron.innovationId -> {
											if(Random.nextDouble < 0.9) {
												currentNeuron.copy(biasWeight = neuronToChange.biasWeight + ((Random.nextDouble * 8) - 4) * Random.nextDouble)
											} else {
												currentNeuron
											}
										})
									}

									parent ! Agent.NewChild(new NetworkGenome(genomeNumber, newNeurons, genome.connections), genomeNumber)

							}
		}

    // Add connection
        // <decscription> find two nodes without a connection
        // add the connection.
        // <param> genome -> the genome due to be mutated.
        // <return> genome -> A new copy of the genome with a new connection added.


    def mutateAddConnection (genome: NetworkGenome, genomeNumber: Int) = {



        // first take all the keys / innovation ids for the neurons
				val validSrcNeurons = genome.neurons.keys.toList
				val validDestNeurons = (genome.hiddenNodes ++ genome.outputNodes).keys.toList // Mustn't connect to the input.

				// select two random neurons
				val n1 = validSrcNeurons(Random.nextInt(validSrcNeurons.length))
				val n2 = validDestNeurons(Random.nextInt(validDestNeurons.length))



				// check not already connected locally

				val existingEntry = genome.connections.values.find(te => te.from == n1 && te.to == n2)

				existingEntry match {

					case Some(e) => {
						// these two are already connected so just return the genome.
						// TODO: We can probably have a few goes at this.. say try 4 times if no success then give up.

						parent ! Agent.NewChild(genome.copy(id = genomeNumber), genomeNumber)

					}

					case None => {
						// the connection does not exist already, in this network, check innovation number in case elsewhere.
						innovationAgent  ! Innovation.NewConnectionProposal(n1, n2)
		        		context become mutatingGenomeAddConnection(genome, genomeNumber)
					}
				}
    }

    /* <Description> mutateAddNeuron: As generally described by the hyperneat papers pick a connection, disable it
                     create two new connections and a new node inbetween.
     <param> NetworkGenome: the network genome to be mutated
     <return> NetworkGenome: A genome with a new neuron added.
    */

    def mutateAddNeuron (genome: NetworkGenome, genomeNumber: Int) = {

        // First, pick a connection from the genome to split.. Randomly...

		// Avoid splitting a recurrent connection.. for now... In theory it creates unnecessary structure..
		// though perhaps fine tunes the signal and seems at face value to converge quicker. it's behaviour is a little difficult to reason about in terms of recurrency
        val connIds: List[Int] = genome.connections.filter(x=> !x._2.recurrent).keys.toList
		//val connIds: List[Int] = genome.connections.keys.toList

        val connToReplace: Int = connIds(Random.nextInt(connIds.length))
        val connectionToSplit: ConnectionGenome = genome.connections(connToReplace)

        //println("split conn innov id: " + connectionToSplit.innovationId + ", which breaks " + connectionToSplit.from + " and " + connectionToSplit.to)
        // Ask the innovation Actor if anyone has already split this connection. If yes, we should use
        // the same innovation id of both neuron and the two connections

        innovationAgent  ! Innovation.NewNeuronProposal(connectionToSplit.from, connectionToSplit.to)
        context become mutatingGenome(genome, connectionToSplit.innovationId, genomeNumber)
    }


    def mutatingGenome(genome: NetworkGenome, oldConnection: Int, genomeNumber: Int): Receive = {

        case Innovation.NewNeuronConfirmation(neuronData) =>
            // using the neuron data change the NetworkGenome
            //println("got some innovation id data... lets mutate that neuron...")

            val oldConnectionGenome: ConnectionGenome = genome.connections(oldConnection)

            val newCons = genome.connections +
                    (oldConnection -> oldConnectionGenome.copy(enabled = false),
                     neuronData.connection1 -> new ConnectionGenome(neuronData.connection1, neuronData.fromNeuron, neuronData.newNeuron),   //new ConnectionGenomes
                     neuronData.connection2 -> new ConnectionGenome(neuronData.connection2, neuronData.newNeuron, neuronData.toNeuron))  //new ConnectionGenomes

			val activationFns = List("SIGMOID", "GAUSSIAN", "SINE")
			val activationFn = activationFns(Random.nextInt(activationFns.length))


            val newNeurons = genome.neurons + (neuronData.newNeuron-> new NeuronGenome(
                    neuronData.newNeuron,
                    activationFn, // In case of CPPN Needs to be randomly selected (WEll, it can also be random for Normal ANN)
                    "hidden",  // Assume we can't ad or remove inputs or outputs.
                    -1, // Bias val
                    Random.nextDouble, // Bias weight
                    (genome.neurons(oldConnectionGenome.from).layer + genome.neurons(oldConnectionGenome.to).layer) / 2 // Layer. SHould be the sum of the layers of the 2 neurons previously conected / 2
                    ))


            context.parent ! Agent.NewChild(new NetworkGenome(genomeNumber, newNeurons, newCons), genomeNumber)
            //println(newCons.toString)

    }

		def mutatingGenomeAddConnection(genome: NetworkGenome, genomeNumber: Int): Receive = {

				case Innovation.NewConnectionConfirmation(connectionData) =>

					//if({ genome.neurons(connectionData.fromNeuron).layer >= genome.neurons(connectionData.toNeuron).layer })
					//	println("creating a recurrent connection")

					val newConnections = genome.connections + (connectionData.innovationId ->
						new ConnectionGenome(
						connectionData.innovationId,
						connectionData.fromNeuron,  //from
						connectionData.toNeuron,  // to
						Random.nextDouble, //weight
						true, // enabled
						{ genome.neurons(connectionData.fromNeuron).layer >= genome.neurons(connectionData.toNeuron).layer })) // recurrent

	 			context.parent ! Agent.NewChild(new NetworkGenome(genomeNumber, genome.neurons, newConnections), genomeNumber)

		}



}
