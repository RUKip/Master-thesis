package com.example.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.Behaviors
import com.example.{Solution, Variable}
import com.example.actors.Node.{Event, ListingResponse}

class SolutionNode {

}

object SolutionNode {
  sealed trait SolutionEvent //Scalas enum
  final case class ReceiveSolution() extends SolutionEvent

  def apply(solution: Solution): Behavior[Event] = Behaviors.setup { context =>
    val NodeServiceKey: ServiceKey[Event] = ServiceKey[Event](solution.id)
    context.system.receptionist ! Receptionist.Register(NodeServiceKey, context.self)

    //Defines what message is responded after the actor is requested from the receptionist
    val listingAdapter: ActorRef[Receptionist.Listing] =
      context.messageAdapter { listing => ListingResponse(listing)}

    receive(solution)
  }

  def receive(solution: Solution): Behavior[Event] = {
    solution.parent.tree_childeren.foreach( node =>
      solution.color_mapping[]

    )
    Behaviors.receive { (context, message) =>
      message match {
        case ReceiveSolution() =>

      }

  }
}