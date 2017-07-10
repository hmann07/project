package com.neurocoevo.agent

import com.neurocoevo.genome._
import com.neurocoevo.neuron._
import com.neurocoevo.substrate.SubstrateNode
import com.neurocoevo.network._
import com.neurocoevo.experience._

import com.neurocoevo.population._
import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}

object Agent {

	def props(cppnGenome: NetworkGenome, experience: ActorRef): Props = Props(new Agent(cppnGenome, experience))


}


/* Agent. 
		Agent will have a genome that defines a CPPN.
		using the Genome it must set up the actors for the network set up the connections and 
		

*/

class Agent(cppnGenome: NetworkGenome, experience: ActorRef) extends Actor with ActorLogging {
	import context._

	println("actor created")

	val ann = actorOf(Network.props(cppnGenome), "ann")

	
	def receive = {
    	
    	case "NetworkReady" =>

    		// Network is ready, lets percieve some "things"
    		experience ! "perceive"
    		//sender() ! Network.Sensation(1, List(1, 0), List(1))

    	case "propagated" =>
    		
    		// finished learning form that sensation... give me another.
    		experience ! "perceive"
    		//sender() ! Network.Sensation(1, List(1, 0), List(1))



    	case Experience.Event(e, l) =>
    		//println(e)
    		ann ! Network.Sensation(1, e, l)


        // Received when a network has completed one signal of a pattern and needs another
        case "newSignal" =>
            println("newsg request")
            // finished processing sensation... give me another.
            experience ! "perceive"
            

        //  Received when a network has processed all expected patterns from a test set.    
        case Network.Matured(g, error) =>
            println("agent got final network data")
            parent ! Network.Matured(g, error)


        // The agent should control Mutations... it will neeed to decide what it is mutating..i.e. CPPN/ANN. not tha tit matters directly. More jus tthat
        // One of them will be being mutated.

        case "Mutate" =>
            ann ! "AddConnection"

  	}



  	

}