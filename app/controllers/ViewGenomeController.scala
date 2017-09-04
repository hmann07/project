package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import java.io.File
import java.io.PrintWriter


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
class ViewGenomeController @Inject()(system: ActorSystem, cc: ControllerComponents) extends AbstractController(cc) {

  def index(filename: String) = Action { implicit request: Request[AnyContent] =>


  		val files = new File(".\\public\\logs\\")

  		val statsFile = new PrintWriter(new File(".\\public\\logs\\statsData.js"))
  		 statsFile.write("var d = " + scala.io.Source.fromFile(".\\public\\logs\\populationStats.js").getLines.toList.mkString("[", ", ", "]"))
  		 statsFile.close

       Ok(views.html.evolutionOutput(files.listFiles.toList, filename))
  }

}
