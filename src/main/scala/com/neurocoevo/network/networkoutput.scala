package com.neurocoevo.network

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}
import java.io.File
import java.io.PrintWriter


import com.neurocoevo.genome.NetworkGenome

object NetworkOutput{

	case class OutputRequest(genome: NetworkGenome, name: String, outputType: String)

}

class NetworkOutput extends Actor with ActorLogging {
	import context._
	import NetworkOutput._


	def receive = {

		case OutputRequest(genome, name, "JSON") =>

			val neuronsJson =  genome.neurons.map(n => "{id: " + n._2.innovationId + ", reflexive: false, layer: " +  n._2.layer + ", actFn: \"" +  n._2.activationFunction + "\"}").mkString("nodes:[", ", \n ",  "],")
			val connectionsJson =  genome.connections.map(n => "{source: " + n._2.from +", target: " + n._2.to +" , left: false, right: true, weight: " + n._2.weight +" } ").mkString("links:[", ", \n ",  "]")


			//val pw = new PrintWriter(new File("C:\\Users\\HMann\\Desktop\\project-master (9)\\project-master\\src\\main\\web\\js\\best-" + genome.id + ".json" ))
			val pw = new PrintWriter(new File(".\\src\\main\\web\\js\\" + name + ".json" ))

			//val pw = new PrintWriter(new File("C:\\Users\\Henry\\Downloads\\project-master\\project-master\\src\\main\\web\\js\\best.json" ))

			try pw.write("var genome = {" + neuronsJson + connectionsJson + "}") finally pw.close()

	}

}
