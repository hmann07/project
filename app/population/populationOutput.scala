package com.neurocoevo.population

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}
import java.io.File
import java.io.PrintWriter
import java.io.FileOutputStream
import com.neurocoevo.parameters.OutputParameters

object PopulationOutput{

	case class PopOutputRequest(content: Map[String, Double], name: String, outputType: String)

}

class PopulationOutput extends Actor with ActorLogging {
	import context._
	import PopulationOutput._


	def receive   = 
	{

		case PopOutputRequest(content, name, "JSON") =>
		
			
			val jsonString = content.map(c => "" + c._1 + ": " + c._2 + "").mkString("{", ", ",  "}")

			val params = OutputParameters()
			val pw = new PrintWriter(new FileOutputStream(new File(params.popOutputPath + name + ".js"), true))

			try {
				
				pw.append(jsonString +"\r\n") 
				
			}
			finally pw.close()

	}

}
