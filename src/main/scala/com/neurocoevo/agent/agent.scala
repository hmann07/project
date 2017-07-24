package com.neurocoevo.agent

import com.neurocoevo.genome._
import com.neurocoevo.neuron._
import com.neurocoevo.substrate.SubstrateNode
import com.neurocoevo.network._
import com.neurocoevo.experience._
import com.neurocoevo.population._
import com.neurocoevo.innovation._

import scala.util.Random

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}

object Agent {

	def props(cppnGenome: NetworkGenome, experience: ActorRef): Props = Props(new Agent(cppnGenome, experience))

    case class NewChild(genome: NetworkGenome, name: String)
}


/* Agent.
		Agent will have a genome that defines a CPPN.
		using the Genome it must set up the actors for the network set up the connections and


*/

class Agent(cppnGenome: NetworkGenome, experience: ActorRef) extends Actor with ActorLogging {
	import context._
    import Agent._
	//println("actor created")

	val ann = actorOf(Network.props(cppnGenome), "ann")


	def receive = {

    	case "NetworkReady" =>

    		// Network is ready, lets percieve some "things"
    		experience ! "perceive"


    	case "propagated" =>

    		// finished learning form that sensation... give me another.
    		experience ! "perceive"


    	case Experience.Event(e, l) =>
    		//println(e)
    		ann ! Network.Sensation(1, e, l)


        // Received when a network has completed one signal of a pattern and needs another
        case "newSignal" =>
            //println("newsg request")
            // finished processing sensation... give me another.
            experience ! "perceive"


        //  Received when a network has processed all expected patterns from a test set.
        case Network.Matured(g, error) =>
            //println("agent got final network data")
            parent ! Network.Matured(g, error)

        // receieved some instructions for crossing over two genomes..
        case Population.Crossover(g, f) =>

            // and add a mutation of some sort...
            //mutateAddNeuron(f(g))
						mutatePerturbWeight(g(0).crossOverGenomes)


            //parent.actorOf(Agent.props(f(g), experience), "agent"+ i + ".1")

  	}

    // MUTATIONS
    // Genrally it seems mutation does not get applied until cross over has happened...
		// Even further implemenations of Hyper neat appear to not mutate when corssover has happended...

    // Perturb weights
        // find a connection
        // vary its weight slightly. or a lot. or by something....

			def mutatePerturbWeight(genome: NetworkGenome) = {
					val connections = genome.connections.values.toList
					val totalConnections = connections.length
					val connectionToChange = connections(Random.nextInt(totalConnections))

					// Could insert some sort of factor here to control how much it changes. also in sharpNeat and Erlang there is a weight cap
					val perturbedConnection = connectionToChange.copy(weight = connectionToChange.weight + (Random.nextDouble * 2) - 1.0)

					val newConnections = genome.connections + (perturbedConnection.innovationId -> perturbedConnection )


					val naming = {
						val t = self.path.name.split('.')
						if (t.length == 2) {
							t(0) + "." + t(1) + ".1"
						} else {
							val incr = (t(2).toInt + 1)
							t(0) + "." + t(1) + "." + incr
						}
					}

	 			parent ! NewChild(new NetworkGenome(genome.neurons, newConnections), naming)
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

    def mutateAddConnection (genome: NetworkGenome) = {

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
						parent ! NewChild(genome, self.path.name)

					}

					case None => {
						// the connection does not exist already, in this network, check innovation number in case elsewhere.
						context.actorSelection("../../innovation")  ! Innovation.NewConnectionProposal(n1, n2)
		        context become mutatingGenomeAddConnection(genome)
					}
				}
    }

    /* <Description> mutateAddNeuron: As generally described by the hyperneat papers pick a connection, disable it
                     create two new connections and a new node inbetween.
     <param> NetworkGenome: the network genome to be mutated
     <return> NetworkGenome: A genome with a new neuron added.
    */

    def mutateAddNeuron (genome: NetworkGenome) = {

        // First, pick a connection from the genome to split.. Randomly...

        val connIds: List[Int] = genome.connections.keys.toList
        val connToReplace: Int = connIds(Random.nextInt(genome.connections.size))
        val connectionToSplit: ConnectionGenome = genome.connections(connToReplace)

        //println("split conn innov id: " + connectionToSplit.innovationId + ", which breaks " + connectionToSplit.from + " and " + connectionToSplit.to)
        // Ask the innovation Actor if anyone has already split this connection. If yes, we should use
        // the same innovation id of both neuron and the two connections

        context.actorSelection("../../innovation")  ! Innovation.NewNeuronProposal(connectionToSplit.from, connectionToSplit.to)
        context become mutatingGenome(genome, connectionToSplit.innovationId)
    }


    def mutatingGenome(genome: NetworkGenome, oldConnection: Int): Receive = {

        case Innovation.NewNeuronConfirmation(neuronData) =>
            // using the neuron data change the NetworkGenome
            //println("got some innovation id data... lets mutate that neuron...")

            val oldConnectionGenome: ConnectionGenome = genome.connections(oldConnection)

            val newCons = genome.connections +
                    (oldConnection -> oldConnectionGenome.copy(enabled = false),
                     neuronData.connection1 -> new ConnectionGenome(neuronData.connection1, neuronData.fromNeuron, neuronData.newNeuron),   //new ConnectionGenomes
                     neuronData.connection2 -> new ConnectionGenome(neuronData.connection2, neuronData.newNeuron, neuronData.toNeuron))  //new ConnectionGenomes


            val newNeurons = genome.neurons + (neuronData.newNeuron-> new NeuronGenome(
                    neuronData.newNeuron,
                    "SIGMOID", // In case of CPPN Needs to be randomly selected
                    "hidden",  // Assume we can't ad or remove inputs or outputs.
                    -1, // Bias val
                    Random.nextDouble, // Bias weight
                    (genome.neurons(oldConnectionGenome.from).layer + genome.neurons(oldConnectionGenome.to).layer) / 2 // Layer. SHould be the sum of the layers of the 2 neurons previously conected / 2
                    ))


            val naming = {
                val t = self.path.name.split('.')

                if (t.length == 2) {
                    t(0) + "." + t(1) + ".1"
                } else {
                    val incr = (t(2).toInt + 1)
                    t(0) + "." + t(1) + "." + incr
                }

            }
            parent ! NewChild(new NetworkGenome(newNeurons, newCons), naming)
            //println(newCons.toString)

    }

		def mutatingGenomeAddConnection(genome: NetworkGenome): Receive = {

				case Innovation.NewConnectionConfirmation(connectionData) =>

					val newConnections = genome.connections + (connectionData.innovationId ->
						new ConnectionGenome(
						connectionData.innovationId,
						connectionData.fromNeuron,  //from
						connectionData.toNeuron,  // to
						Random.nextDouble, //weight
						true, // enabled
						{ genome.neurons(connectionData.fromNeuron).layer >= genome.neurons(connectionData.fromNeuron).layer })) // recurrent

					val naming = {
						val t = self.path.name.split('.')
						if (t.length == 2) {
							t(0) + "." + t(1) + ".1"
						} else {
							val incr = (t(2).toInt + 1)
							t(0) + "." + t(1) + "." + incr
						}
					}

	 			parent ! NewChild(new NetworkGenome(genome.neurons, newConnections), naming)



		}



}
