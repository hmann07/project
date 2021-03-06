package com.neurocoevo.network

import com.neurocoevo.genome._
import com.neurocoevo.neuron._
import com.neurocoevo.activationfunction.ActivationFunction
import com.neurocoevo.substrate.SubstrateNode
import com.neurocoevo.parameters.BackPropParameters
import com.neurocoevo.parameters.EvaluatorParameters
import com.neurocoevo.evolution.Evaluator

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}

import scala.collection.immutable.HashMap
import scala.collection.immutable.SortedMap

object Network {

	case class Output(value: Double, signalType: String)

	case class Matured(genome: NetworkGenome, error: Double, sse: Double)

	case class NetworkSettings(
		confirmedConnections: Int = 0,
		sensations: Map[Double, Sensation] = Map.empty,
    	totalSensationsReceived: Int = 0,
		confirmedPropagations: Int = 0,
    	outputsReceived: SortedMap[Int, Double] = SortedMap.empty,
    	sse: Double = 0,
    	fitnessValue: Double = 0.00000000001, // tiny, so that we avoid any nasty divide by zero errors.. 
      maxIterations: Int = BackPropParameters().maxIterations,
      evaluator: Evaluator = EvaluatorParameters().evaluator.evaluateEpoch() )

	case class Sensation(
			 id: Double,
			 values: List[Double],
			 label: List[Double],
       		 signalType: String)

	case class Error(error:Double)

	case class NetworkReady(genome: NetworkGenome)

	case class NetworkOutput(outputs: SortedMap[Int, Double])

	def props(genome: NetworkGenome): Props = Props(new Network(genome))

}

class Network(genome: NetworkGenome) extends Actor with ActorLogging {
	import context._
	import Network._

  override def postStop() {

        
        context.children.foreach(c=> context.stop(c))

    }

	// create actors for nodes

	val inputs: SortedMap[String, ActorRef] = generateInputNeurons( genome.inputNodes, SortedMap.empty)
	val outputs: SortedMap[String, ActorRef] = generateOutputNeurons( genome.outputNodes, SortedMap.empty)
	val hidden: SortedMap[String, ActorRef] = generateHiddenNeurons( genome.hiddenNodes, SortedMap.empty)
	val allnodes: SortedMap[String, ActorRef] = inputs ++ hidden ++ outputs

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
    				parent ! NetworkReady(genome)
    				context become readyNetwork(settings.copy(confirmedConnections = settings.confirmedConnections + 1))
    			} else {
    				//println("received confirmatio1n of Connection")
    				context become initialisingNetwork(settings.copy(confirmedConnections = settings.confirmedConnections + 1))
    			}
    		}
  	}


    // State: ready. Can take signals, deal with outputs coming from neurons and propagated error.


  	def readyNetwork(settings: NetworkSettings) : Receive = {

  		case Sensation(id, v, l, signalType) =>
  			  			
        v.zip(inputs).foreach({case (v,i) => i._2 ! Neuron.Signal(v, false, signalType)})
  			
        context become readyNetwork(settings.copy(sensations = settings.sensations + (id -> Sensation(id, v, l, signalType)),
                                                    totalSensationsReceived = settings.totalSensationsReceived + 1))


      // What happens here is important decision point. If we are going to back propagate error then the output eerror should be
      // sent back through the network and next next signal is called for. If we are going to terminate and start genetic operations then we need to
      // wait until all signals have gone through (relaxing the network on each pass), calculate performance and tell the agent the network is done.


	  case Output(v, "ANNCONFIG") =>

	    //println("ANN CONFIG sig propagated.")

      val updatedOutputs: SortedMap[Int, Double] = settings.outputsReceived + (sender().path.name.toInt -> v)

      if(updatedOutputs.size == outputs.size) {

        context.actorSelection("../annFac") ! NetworkOutput(updatedOutputs)

        context become readyNetwork(settings.copy(outputsReceived = SortedMap.empty))

      } else {

        // wait for more network outputs
        context become readyNetwork(settings.copy(outputsReceived = updatedOutputs))

      }

	  case Output(v, "TEST") =>
        println("TEST sig propagated.")
        //context.actorSelection("../ann") ! v

	  case Output(v, "EVOLVE") =>

        val updatedOutputs: SortedMap[Int, Double] = settings.outputsReceived + (sender().path.name.toInt -> v)

        if(updatedOutputs.size == outputs.size) {
          // All Network outputs have been received

          val updatedEval: Evaluator = settings.evaluator.evaluateIteration(settings.sensations(1), updatedOutputs)

          settings.totalSensationsReceived match {
            case updatedEval.epochLength => {

              val updatedEvalEpoch = updatedEval.evaluateEpoch()

              parent ! Matured(genome,   updatedEvalEpoch.fitness , updatedEvalEpoch.auxValue )
              
              // Don't need to relax since all sensations have finished. 
              // the actor will shortly cease to be. so no real need to do anything.. This also causes deadletters.
              
              context become readyNetwork(settings.copy(evaluator = updatedEvalEpoch))
            }
        
            case _ => {
              // rest network, which will, once complete, ask for move signals.
              children.foreach(c => c ! "Relax")
              context become  relaxingNetwork(settings.copy(evaluator = updatedEval, outputsReceived = SortedMap.empty ), 0)
            }
          }



        } else {
          // Still waiting for outputs.
           context become readyNetwork(settings.copy(outputsReceived = updatedOutputs))
        }

        


    case Output(v, "BP") =>
			
        // If we are in Back propagation mode.
       
        val error = settings.sensations(1).label(0) - v
        val squaredError = math.pow(error, 2)


        settings.totalSensationsReceived % 4 match {
          case 0 => {

            // This is the end of the epoch
            // no need to send every single epoch, wait for a multiple of 10.
            if(settings.totalSensationsReceived % 40 == 0){
              // Not elegant, cheating by throwing the epoch in the fitness value field...
              parent ! Matured(genome, settings.totalSensationsReceived / 4, settings.sse + squaredError)
            }

            if(settings.totalSensationsReceived == settings.maxIterations){
              // Not elegant, cheating by throwing the epoch in the fitness value field...
              //STOP 

            } else {

            // backwards propagate error
            sender() ! Error(error)

            // reset the squared error for pattern. ready for next iteration
            context become  readyNetwork(settings.copy(sse = 0))
          }
          }
          
         // case 2 =>
         //   println(error)
         //   println(settings.totalSensationsReceived)

          case _ => {

            //println(error)
            //println(settings.totalSensationsReceived)
            sender() ! Error(error)

            context become  readyNetwork(settings.copy(sse = settings.sse + squaredError))
          }
        }
   


  		case "propagated" =>
  			//println("error propagated")
  			if ( settings.confirmedPropagations + 1 == inputs.size){
  				//println(genome)
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
        context become snapshotting(settings, 0, NetworkGenome(0,null,null))

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

    def generateInputNeurons(neurons: HashMap[Int, NeuronGenome], agg: SortedMap[String, ActorRef]  ): SortedMap[String, ActorRef] = {
      neurons.size match {
        case 0 => agg
        case _ => generateInputNeurons(neurons.tail, agg + (neurons.head._2.innovationId.toString -> actorOf(InputNeuron.props(neurons.head._2.biasWeight, ActivationFunction(neurons.head._2.activationFunction)), neurons.head._2.innovationId.toString)))
      }
    }

    def generateOutputNeurons(neurons: HashMap[Int, NeuronGenome], agg: SortedMap[String, ActorRef]  ): SortedMap[String, ActorRef] = {
      neurons.size match {
        case 0 => agg
        case _ => generateOutputNeurons(neurons.tail, agg + (neurons.head._2.innovationId.toString -> actorOf(OutputNeuron.props(neurons.head._2.biasWeight, ActivationFunction(neurons.head._2.activationFunction)), neurons.head._2.innovationId.toString)))
      }
    }

    def generateHiddenNeurons(neurons: HashMap[Int, NeuronGenome], agg: SortedMap[String, ActorRef]  ): SortedMap[String, ActorRef] = {
      neurons.size match {
        case 0 => agg
        case _ => generateHiddenNeurons(neurons.tail, agg + (neurons.head._2.innovationId.toString -> actorOf(Neuron.props(neurons.head._2.biasWeight, ActivationFunction(neurons.head._2.activationFunction)), neurons.head._2.innovationId.toString)))
      }
    }


}
