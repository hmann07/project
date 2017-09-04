package controllers

import javax.inject._
import play.api._
import play.api.mvc._


import com.neurocoevo.universe._


import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}

// Inject the actor system into the controller 
@Singleton
class StartController @Inject()(system: ActorSystem, cc: ControllerComponents) extends AbstractController(cc) {

  def index() = Action { implicit request: Request[AnyContent] =>

	//val inbox = Inbox.create(system)

	system.actorOf(Props[Universe])

    Ok(views.html.evolutionOutput(null, null))
  }

}
