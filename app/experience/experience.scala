package com.neurocoevo.experience

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}

object Experience {
  
  	case class Event(event: List[Double], meaning: List[Double]) 

  	val events: List[Event] = List(
  		
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,1,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(1, 6)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,1,1,1,0 ,0,0,1,0,0,0,0,1,1,1,0 ,0,0,0,0,0,0,0,1,1,1,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(8, 2)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,1,1,1,0,0,0,0,0 ,0,0,0,1,1,1,0,0,0,1,0 ,0,0,0,1,1,1,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(4, 5)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,1,1,1,0,0 ,0,1,0,0,0,0,1,1,1,0,0 ,0,0,0,0,0,0,1,1,1,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(7, 4)),
Event(List(0,0,0,0,0,0,1,1,1,0,0 ,0,0,0,0,0,0,1,1,1,0,0 ,0,0,0,0,0,0,1,1,1,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,1,0,0,0,0,0,0,0,0,0 ), List(7, 9)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,1,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(2, 5)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,1,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ), List(2, 1)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,1,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ), List(1, 1)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,1,1,1,0,0,0 ,1,0,0,0,0,1,1,1,0,0,0 ,0,0,0,0,0,1,1,1,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(6, 4)),
Event(List(0,0,0,0,1,1,1,0,0,0,1 ,0,0,0,0,1,1,1,0,0,0,0 ,0,0,0,0,1,1,1,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(5, 9)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,1,1,1,0,0,0,0 ,0,0,0,0,1,1,1,0,0,0,1 ,0,0,0,0,1,1,1,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(5, 5)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,1,1,1,0,0,0,0 ,0,0,0,0,1,1,1,0,0,0,1 ,0,0,0,0,1,1,1,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(5, 5)),
Event(List(0,0,0,0,0,0,0,1,1,1,0 ,0,0,1,0,0,0,0,1,1,1,0 ,0,0,0,0,0,0,0,1,1,1,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(8, 9)),
Event(List(0,0,0,0,0,1,1,1,0,0,0 ,0,0,0,0,0,1,1,1,0,0,0 ,0,0,0,0,0,1,1,1,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,1,0,0,0,0,0,0,0,0,0,0 ), List(6, 9)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,1,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(2, 5)),
Event(List(0,0,0,0,0,0,0,1,1,1,0 ,0,0,0,0,0,0,0,1,1,1,0 ,0,0,0,0,0,0,0,1,1,1,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,1,0,0,0,0,0,0,0,0 ), List(8, 9)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,1,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(3, 7)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,1,1,1,0,0 ,0,1,0,0,0,0,1,1,1,0,0 ,0,0,0,0,0,0,1,1,1,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(7, 3)),
Event(List(1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,1,0,0,0,0,0 ), List(1, 9)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,1,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(1, 7)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,1,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(3, 3)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,1,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(3, 7)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,1,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(1, 8)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,1,1,1,0 ,0,0,1,0,0,0,0,1,1,1,0 ,0,0,0,0,0,0,0,1,1,1,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(8, 4)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,1,1,1,0,0,0 ,1,0,0,0,0,1,1,1,0,0,0 ,0,0,0,0,0,1,1,1,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(6, 7)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,1,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,1,1,1 ,0,0,0,0,0,0,0,0,1,1,1 ,0,0,0,0,0,0,0,0,1,1,1 ,0,0,0,0,0,0,0,0,0,0,0 ), List(9, 2)),
Event(List(0,1,1,1,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,1,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(2, 9)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,1,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(1, 4)),
Event(List(0,0,0,0,0,0,0,0,1,1,1 ,0,0,0,0,0,0,0,0,1,1,1 ,0,0,0,0,0,0,0,0,1,1,1 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,1,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(9, 9)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,1 ), List(3, 7)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,1,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,1,1,1,0,0 ,0,0,0,0,0,0,1,1,1,0,0 ,0,0,0,0,0,0,1,1,1,0,0 ), List(7, 1)),
Event(List(0,0,0,0,0,0,0,0,1,1,1 ,0,0,0,0,0,0,0,0,1,1,1 ,0,0,0,0,0,0,0,0,1,1,1 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,1,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(9, 9)),
Event(List(0,0,0,0,0,0,0,0,1,1,1 ,0,0,0,0,0,0,0,0,1,1,1 ,0,0,0,0,0,0,0,0,1,1,1 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,1,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(9, 9)),
Event(List(0,0,0,0,0,0,1,1,1,0,0 ,0,0,0,0,0,0,1,1,1,0,0 ,0,0,0,0,0,0,1,1,1,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,1,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(7, 9)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,1,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(1, 4)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,1,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(1, 5)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,1,1,1,0,0,0,0 ,0,0,0,0,1,1,1,0,0,0,0 ,0,0,0,0,1,1,1,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,1,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(5, 8)),
Event(List(0,0,0,0,0,1,1,1,0,0,0 ,0,0,0,0,0,1,1,1,0,0,0 ,0,0,0,0,0,1,1,1,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,1,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(6, 9)),
Event(List(0,1,1,1,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,1,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(2, 9)),
Event(List(1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,1,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(1, 9)),
Event(List(0,0,0,0,0,0,0,0,0,0,1 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(3, 6)),
Event(List(0,0,0,0,0,0,0,1,1,1,0 ,0,0,0,0,0,0,0,1,1,1,0 ,0,0,0,0,0,0,0,1,1,1,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,1,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(8, 9)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,1,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ), List(2, 1)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,1,1,1 ,0,0,0,0,0,0,0,0,1,1,1 ,0,0,0,0,0,0,0,0,1,1,1 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,1,0,0,0,0,0 ), List(9, 7)),
Event(List(0,0,0,0,0,1,1,1,0,0,0 ,0,0,0,0,0,1,1,1,0,0,0 ,0,0,0,0,0,1,1,1,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,1,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(6, 9)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,1 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(3, 4)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,1,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,1,1,1 ,0,0,0,0,0,0,0,0,1,1,1 ,0,0,0,0,0,0,0,0,1,1,1 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(9, 5)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,1,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(2, 3)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,1,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(2, 4)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,1,0 ), List(2, 7)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,1,1,1,0,0,0 ,0,0,0,0,0,1,1,1,0,0,0 ,0,0,0,0,0,1,1,1,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,1,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(6, 6)),
Event(List(0,1,1,1,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,1,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(2, 9)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,1,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(3, 8)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,1,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ), List(1, 1)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,1,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ), List(2, 1)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,1,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ), List(1, 1)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,1,1,1,0 ,0,0,0,0,0,0,0,1,1,1,0 ,0,0,0,0,0,0,0,1,1,1,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,1,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(8, 6)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,1,1,1,0,0,0 ,0,0,0,0,0,1,1,1,0,0,0 ,0,0,0,0,0,1,1,1,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,1,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(6, 8)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,1,1,1,0,0,0,0 ,0,0,0,0,1,1,1,0,0,0,0 ,0,0,0,0,1,1,1,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,1,0,0,0,0,0 ), List(5, 5)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,1,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(3, 3)),
Event(List(0,0,0,0,0,0,0,0,1,1,1 ,0,0,0,0,0,0,0,0,1,1,1 ,0,0,0,0,0,0,0,0,1,1,1 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,1,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(9, 9)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,1,1,1,0,0,0,0 ,0,0,0,0,1,1,1,0,0,0,0 ,0,0,0,0,1,1,1,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,1,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(5, 8)),
Event(List(0,0,0,0,1,1,1,0,0,0,0 ,0,0,0,0,1,1,1,0,0,0,0 ,0,0,0,0,1,1,1,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,1,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(5, 9)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,1,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,1,1,1,0,0,0,0 ,0,0,0,0,1,1,1,0,0,0,0 ,0,0,0,0,1,1,1,0,0,0,0 ), List(5, 1)),
Event(List(0,0,0,0,1,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,1,1,1,0,0,0,0,0 ,0,0,0,1,1,1,0,0,0,0,0 ,0,0,0,1,1,1,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(4, 4)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,1,1,1,0,0,0 ,0,0,0,0,0,1,1,1,0,0,0 ,0,0,0,0,0,1,1,1,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,1,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(6, 6)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,1 ), List(1, 5)),
Event(List(1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,1 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(1, 9)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,1,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(3, 3)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,1,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,1,1,1,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(3, 2)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,1 ), List(1, 5)),
Event(List(0,0,0,0,0,0,1,1,1,0,0 ,0,0,0,0,0,0,1,1,1,0,0 ,0,0,0,0,0,0,1,1,1,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,1,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(7, 9)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,1,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ), List(2, 1)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,1,1,1,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,1,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(1, 7)),
Event(List(0,0,0,0,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,1,1,1,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,1,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ,0,0,0,0,0,0,0,0,0,0,0 ), List(2, 8)))

  	case class ExperienceSettings(experiencesServed: Int = 0)

}


class Experience extends Actor with ActorLogging {
	import Experience._
	import context._

	def receive =  servePerceptions(ExperienceSettings())

	def servePerceptions(s: ExperienceSettings): Receive = {
		
		case "perceive" =>
			//println("experience #" + (s.experiencesServed + 1) + events(s.experiencesServed % (events.length)))
			sender ! events(s.experiencesServed % (events.length))
			context become servePerceptions(s.copy(experiencesServed = s.experiencesServed + 1))

		case "STOP" =>
			context stop self
	}
}