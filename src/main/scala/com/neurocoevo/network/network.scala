package com.neurocoevo.network

import com.neurocoevo.genome._
import com.neurocoevo.neuron._
import com.neurocoevo.substrate.SubstrateNode

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}

object Network {

	case class Output(value: Double)

	case class NetworkSettings(
			confirmedConnections: Int = 0,
			sensations: Map[Double, Sensation] = Map.empty,
      totalSensationsReceived: Int = 0,
			confirmedPropagations: Int = 0,
      
      tss: Double = 0

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
	val totalConnections: Int = genome.connections.length 

	// create connections based on actor references
	val actorReferencedConnections = genome.connections.map {c => new ActorConnection(allnodes(c.from.toString),allnodes(c.to.toString), c.weight)}

	// inform all neurons about their incoming and outgoing connections.
	actorReferencedConnections.foreach {c =>
		c.source ! Neuron.Destination(Map(c.destination -> c.weight))
		c.destination ! Neuron.Source(Map(c.source -> c.weight))
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
    				//println("received confirmation of Connection")
    				context become initialisingNetwork(settings.copy(confirmedConnections = settings.confirmedConnections + 1))
    			}
    		}
  	}

  	def readyNetwork(settings: NetworkSettings) : Receive = {
  		
  		case Sensation(id, v, l) => 
  			//println("preparing to send, expected value: " + l)
  			v.zip(inputs.values).foreach({case (v,i) => i ! Neuron.Signal(v)})
  			context become readyNetwork(settings.copy(sensations = settings.sensations + (id -> Sensation(id, v, l)),
                                                    totalSensationsReceived = settings.totalSensationsReceived + 1))
  		
  		case Output(v) =>

        val error = settings.sensations(1).label(0) - v
        val squaredError = math.pow(error, 2)

        settings.totalSensationsReceived % 4 match {
          case 0 => {
            println(parent.path.name + ", " + settings.totalSensationsReceived + ", " + (settings.tss + squaredError))
            sender() ! Error(error)
            context become  readyNetwork(settings.copy(tss = 0))
          }
          case _ => {
            sender() ! Error(error)
            context become  readyNetwork(settings.copy(tss = settings.tss + squaredError))
          }
        }
        
  			
  			

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

  		case Neuron.NeuronSettings(activationFunction,
    							   	signal,
    							  	outputs,
    	                           	inputs,
    								signalsReceived,
    								learningRate,
    								errorGradientsReceived,
    								totalErrorGradient,
    								biasValue,
    								biasWeight) =>

  			

  			


  	}


    /*
    Not ideal to have three so similar. could introduce the Neuron class as part of the parameter.
      Generate Neurons: Take a list of SubstrateNodes and create actors for each one.
      signature: List of nodes -> Map (actor name -> actor)
    */

    def generateInputNeurons(neurons: List[NeuronGenome], agg: Map[String, ActorRef]  ): Map[String, ActorRef] = {
      neurons.length match {
        case 0 => agg
        case _ => generateInputNeurons(neurons.tail, agg + (neurons.head.innovationId.toString -> actorOf(Props[InputNeuron], neurons.head.innovationId.toString)))
      }
    }

    def generateOutputNeurons(neurons: List[NeuronGenome], agg: Map[String, ActorRef]  ): Map[String, ActorRef] = {
      neurons.length match {
        case 0 => agg
        case _ => generateOutputNeurons(neurons.tail, agg + (neurons.head.innovationId.toString -> actorOf(Props[OutputNeuron], neurons.head.innovationId.toString)))
      }
    }

    def generateHiddenNeurons(neurons: List[NeuronGenome], agg: Map[String, ActorRef]  ): Map[String, ActorRef] = {
      neurons.length match {
        case 0 => agg
        case _ => generateHiddenNeurons(neurons.tail, agg + (neurons.head.innovationId.toString -> actorOf(Props[Neuron], neurons.head.innovationId.toString)))
      }
    }


}