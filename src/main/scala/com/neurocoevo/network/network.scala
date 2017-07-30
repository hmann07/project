package com.neurocoevo.network

import com.neurocoevo.genome._
import com.neurocoevo.neuron._
import com.neurocoevo.substrate.SubstrateNode

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}

import scala.collection.immutable.HashMap

object Network {

	case class Output(value: Double)
  case class Matured(genome: NetworkGenome, error: Double, sse: Double)

	case class NetworkSettings(
		confirmedConnections: Int = 0,
		sensations: Map[Double, Sensation] = Map.empty,
      	totalSensationsReceived: Int = 0,
		confirmedPropagations: Int = 0,
      	sse: Double = 0,
      	fitnessValue: Double = 0.00000000001,
		performanceFunction: ((Double, Double) => Double) = (networkOutput: Double, expectedValue: Double) => math.pow(expectedValue - networkOutput, 2)
		)
	case class Sensation(
			 id: Double,
			 values: List[Double],
			 label: List[Double])

	case class Error(error:Double)

	def props(genome: NetworkGenome): Props = Props(new Network(genome))

}

class Network(genome: NetworkGenome) extends Actor with ActorLogging {
	import context._
	import Network._

	// create actors for nodes
	val inputs: Map[String, ActorRef] = generateInputNeurons( genome.inputNodes, Map.empty)
	val outputs: Map[String, ActorRef] = generateOutputNeurons( genome.outputNodes, Map.empty)
	val hidden: Map[String, ActorRef] = generateHiddenNeurons( genome.hiddenNodes, Map.empty)
	val allnodes: Map[String, ActorRef] = inputs ++ hidden ++ outputs

	val totalConnections: Int = genome.connections.size
	
	// create connections based on actor references
	val actorReferencedConnections = genome.connections.map {c => new ActorConnection(
		c._2.innovationId,
		allnodes(c._2.from.toString),
		allnodes(c._2.to.toString),
		c._2.weight,
		{if(!c._2.recurrent){
				// just double check... but also catch genomes from substrate that might not define them properly.
				genome.neurons(c._2.from).layer  >= genome.neurons(c._2.to).layer
			} else {
				c._2.recurrent // should always be true.
			}
		})}

	// inform all neurons about their incoming and outgoing connections.
	actorReferencedConnections.foreach {c =>
		c.source ! Neuron.Destination(Map(c.destination -> Neuron.ConnectionDetail(c.innovationId, c.weight, c.recurrent)))
		c.destination ! Neuron.Source(Map(c.source -> Neuron.ConnectionDetail(c.innovationId, c.weight, c.recurrent)))
	}

	//println(actorReferencedConnections)



  	def receive = initialisingNetwork(NetworkSettings())

  	// State: initialising. Will be waiting for confirmation from all neurons that they have received their
  	// connections.

  	def	initialisingNetwork(settings: NetworkSettings): Receive = {

    	case "ConnectionConfirmation"  => {
    			//println(totalConnections)
    			if(settings.confirmedConnections + 1 == (totalConnections * 2)){
    				//println("received confirmation of all Connections")
    				children.foreach(c => c ! "Init Complete")
    				parent ! "NetworkReady"
    				context become readyNetwork(settings.copy(confirmedConnections = settings.confirmedConnections + 1))
    			} else {
    				//println("received confirmatio1n of Connection")
    				context become initialisingNetwork(settings.copy(confirmedConnections = settings.confirmedConnections + 1))
    			}
    		}
  	}


    // State: ready. Can take signals, deal with outputs coming from neurons and propageted error.


  	def readyNetwork(settings: NetworkSettings) : Receive = {

  		case Sensation(id, v, l) =>
  			//println("preparing to send, expected value: " + l)
  			v.zip(inputs.values).foreach({case (v,i) => i ! Neuron.Signal(v, false)})
  			context become readyNetwork(settings.copy(sensations = settings.sensations + (id -> Sensation(id, v, l)),
                                                    totalSensationsReceived = settings.totalSensationsReceived + 1))


      // What happens here is important decision point. If we are going to back propagate error then the output eerror should be
      // sent back through the network and next next signal is called for. If we are going to terminate and start genetic operations then we need to
      // wait until all signals have gone through (relaxing the network on each pass), calculate performance and tell the agent the network is done.

      case Output(v) =>

        val error = settings.sensations(1).label(0) - v
        val squaredError = math.pow(error, 2)

				// If we are in Back propagation mode.
        /*
				settings.totalSensationsReceived % 4 match {
          case 0 => {
            if(settings.totalSensationsReceived % 10 == 0){
							val ts = System.currentTimeMillis()
              println(parent.path.name + ", " + settings.totalSensationsReceived + ", " + (settings.sse + squaredError) + ", " + ts)
            }

						// backwards propagate error
            sender() ! Error(error)

						// reset the squared error for pattern. ready for next iteration
            context become  readyNetwork(settings.copy(sse = 0))
          }
          case _ => {

						sender() ! Error(error)

            context become  readyNetwork(settings.copy(sse = settings.sse + squaredError))
          }
        }
		*/


        // If we are in Evolution Only mode.
			//	/*

				val fitnessValue = settings.performanceFunction(v, settings.sensations(1).label(0))

        settings.totalSensationsReceived match {
          case 4 => {
						val ts = System.currentTimeMillis()
						parent ! Matured(genome,   1 / ((settings.fitnessValue + fitnessValue)), settings.sse )
						//println(parent.path.name + ", " + settings.totalSensationsReceived + ", " + {(settings.sse + fitnessValue) / settings.totalSensationsReceived }+ ", " + v + ", " + settings.sensations(1).label(0))
            // Don't need to relax since all sensations have finished. This also causes deadletters.
						// children.foreach(c => c ! "Relax")

						context become readyNetwork(settings.copy(sse = 0, fitnessValue = 0))
          }
          case _ => {
						//println(parent.path.name + ", " + settings.totalSensationsReceived + ", " + {(settings.sse + fitnessValue) / settings.totalSensationsReceived } + ", " + v + ", " + settings.sensations(1).label(0))
            children.foreach(c => c ! "Relax")
            context become  relaxingNetwork(settings.copy(
              sse = settings.sse + squaredError,
              fitnessValue = settings.fitnessValue + fitnessValue), 0)
          }
        }
				//*/


  		case "propagated" =>
  			//println("error propagated")
  			if ( settings.confirmedPropagations + 1 == inputs.size){
  				parent ! "propagated"
  				context become readyNetwork(settings.copy(confirmedPropagations = 0))
  			} else {
  				context become readyNetwork(settings.copy(confirmedPropagations = settings.confirmedPropagations + 1))
  			}

  		// On snapshot Network should send message to all nodes and get outgoing connection data
  		// as well as bias information
  		// and in the case of a CPPN activation function
  		// The data should then be consumed into a genome to be subjected to NEAT operators
  		// The issue here is, if we allow back propagation i.e learning outside of evolution, then
  		// the weights will be updated without the netwrok knowing....
  		// children can just send their settings object and carry on.

  		case "snapshot" =>
  			children.foreach { c => c ! "snapshot" }
        context become snapshotting(settings, 0, NetworkGenome(null,null))

      case "Add Connection" =>

      // here we select a random connection.... then communicate directly with the innovation engine...




  	}

    def snapshotting(s: NetworkSettings, snapshotsReceived: Int, newGenome: NetworkGenome): Receive = {

      case Neuron.NeuronSnapshot(ng, cg) =>

        if(snapshotsReceived + 1 == allnodes.size){

          // this is the last snapshot.

          val g = newGenome.copy(neurons = genome.neurons + ng, connections1 = genome.connections1 ++ cg)

          context become readyNetwork(s)

        } else {

          // still more to wait for

          val g = newGenome.copy(neurons = genome.neurons + ng, connections1 = genome.connections1 ++ cg)

          context become snapshotting(s, snapshotsReceived + 1 , g)

        }
    }


    def relaxingNetwork(s:NetworkSettings, relaxedConfirmed: Int): Receive = {

      case "NeuronRelaxed" =>

        if(relaxedConfirmed + 1 == allnodes.size){
          parent ! "newSignal"
          context become readyNetwork(s)
        } else {
          context become relaxingNetwork(s, relaxedConfirmed+ 1)
        }

    }




    /*
    Not ideal to have three so similar. could introduce the Neuron class as part of the parameter.
      Generate Neurons: Take a list of SubstrateNodes and create actors for each one.
      signature: List of nodes -> Map (actor name -> actor)
    */

    def generateInputNeurons(neurons: HashMap[Int, NeuronGenome], agg: Map[String, ActorRef]  ): Map[String, ActorRef] = {
      neurons.size match {
        case 0 => agg
        case _ => generateInputNeurons(neurons.tail, agg + (neurons.head._2.innovationId.toString -> actorOf(Props[InputNeuron], neurons.head._2.innovationId.toString)))
      }
    }

    def generateOutputNeurons(neurons: HashMap[Int, NeuronGenome], agg: Map[String, ActorRef]  ): Map[String, ActorRef] = {
      neurons.size match {
        case 0 => agg
        case _ => generateOutputNeurons(neurons.tail, agg + (neurons.head._2.innovationId.toString -> actorOf(Props[OutputNeuron], neurons.head._2.innovationId.toString)))
      }
    }

    def generateHiddenNeurons(neurons: HashMap[Int, NeuronGenome], agg: Map[String, ActorRef]  ): Map[String, ActorRef] = {
      neurons.size match {
        case 0 => agg
        case _ => generateHiddenNeurons(neurons.tail, agg + (neurons.head._2.innovationId.toString -> actorOf(Props[Neuron], neurons.head._2.innovationId.toString)))
      }
    }


}
