package com.example.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import com.example.{Mapping, Solution}

class SolutionNode {

}

object SolutionNode {
  sealed trait SolutionEvent extends Node.Event//Scalas enum
  final case class ReceiveOptimalSolution(solution: Solution) extends SolutionEvent
  final case class SendSolution(color_mapping: Map[Int, String]) extends SolutionEvent
  final case class ListingResponse(listing: Receptionist.Listing, from: Int) extends SolutionEvent
  final case class SendOptimalSolution(solution: Solution) extends SolutionEvent


  def apply(
             solution: Solution,
             mapping: Mapping,
             parent_ref: ActorRef[Node.Event]
           ): Behavior[SolutionEvent] = Behaviors.setup { context =>
    requestChildRef(solution, context)
    receive(parent_ref, solution, mapping, solution)
  }

  def requestChildRef(solution: Solution, context: ActorContext[SolutionEvent]): Unit = {
    //Defines what message is responded after the actor is requested from the receptionist
    val listingAdapter: ActorRef[Receptionist.Listing] =
      context.messageAdapter { listing => {
          val from: Int = listing.getKey.id.toInt
          ListingResponse(listing, from)
        }
      }

    solution.parent.tree_childeren.foreach( node =>
      context.system.receptionist ! Receptionist.Find(ServiceKey[SolutionEvent](node.toString), listingAdapter)
    )
  }

  def sendSolution(solution: Solution, actorRef: ActorRef[SolutionEvent], node_id: Int, mapping: Mapping): Unit ={
    actorRef ! SendSolution(mapping.getSpecificMapping(solution, node_id))
  }

  def receive(parent_ref: ActorRef[Node.Event], solution: Solution, mapping: Mapping, final_solution: Solution): Behavior[SolutionEvent] = {
    val NodeServiceKey: ServiceKey[SolutionEvent] = ServiceKey[SolutionEvent](solution.id)
    var new_final_solution = final_solution
    Behaviors.receive { (context, message) =>
      message match {
        case ListingResponse(NodeServiceKey.Listing(listings), from) =>
          val xs: Set[ActorRef[SolutionEvent]] = listings
          xs foreach { replyTo =>
            sendSolution(solution, replyTo, from, mapping)
          }
        case ReceiveOptimalSolution(optimal_solution) =>
          if (optimal_solution == null) {
            parent_ref ! SendOptimalSolution(null)
            //TODO: should break here
            Behaviors.empty
          }
          new_final_solution = final_solution.addSolution(optimal_solution)
      }
      receive(parent_ref, solution, mapping, new_final_solution)
    }
  }
}