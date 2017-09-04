package controllers

import javax.inject._
import play.api._
import play.api.mvc._


import com.neurocoevo.substrate._
import com.neurocoevo.genome._
import com.neurocoevo.agent.Agent
import com.neurocoevo.experience.Experience
import com.neurocoevo.population.Population
import com.neurocoevo.innovation.Innovation
import com.neurocoevo.parameters._

import akka.actor.ActorSystem
import akka.actor.{Actor, ActorRef, ActorLogging, Props, Inbox}

// Inject the actor system into the controller 
@Singleton
class HomeController @Inject()(system: ActorSystem, cc: ControllerComponents) extends AbstractController(cc) {

  def index() = Action { implicit request: Request[AnyContent] =>

       Ok(views.html.index())
  }

}
