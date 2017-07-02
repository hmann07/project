package com.neurocoevo.experience

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}

object Experience {
  
  	case class Event(event: List[Double], meaning: List[Double]) 

  	val events: List[Event] = List(
  		Event(List(1, 1), List(0)),
  		Event(List(0, 1), List(1)),
  		Event(List(1, 0), List(1)),
  		Event(List(0, 0), List(0)))

  	case class ExperienceSettings(experiencesServed: Int = 0)

}


class Experience extends Actor with ActorLogging {
	import Experience._


	def receive =  servePerceptions(ExperienceSettings())

	def servePerceptions(s: ExperienceSettings): Receive = {
		
		case "perceive" =>
			println("experience #" + (s.experiencesServed + 1) + events(s.experiencesServed % (events.length)))
			sender ! events(s.experiencesServed % (events.length))
			context become servePerceptions(s.copy(experiencesServed = s.experiencesServed + 1))
	}
}