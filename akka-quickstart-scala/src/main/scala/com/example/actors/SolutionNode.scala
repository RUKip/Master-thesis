package com.example.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import com.example.actors.NodeSearch.SendOptimalSolution
import com.example.actors.SolutionNode.{ListingResponse, ReceiveOptimalSolution, SendSolution, SolutionEvent}
import com.example.{Mapping, Solution}

class SolutionNode(val solution: Solution, val mapping: Mapping, val parent_ref: ActorRef[Node.Event], val reference: ActorRef[SolutionEvent]) {

  def requestChildRef(context: ActorContext[SolutionEvent]): Unit = {
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

  def sendSolution(solution: Solution, actorRef: ActorRef[SolutionEvent], node_id: Int): Unit ={
    actorRef ! SendSolution(mapping.getSpecificMapping(solution, node_id))
  }

  def receive(final_solution: Solution): Behavior[SolutionEvent] = {
    val NodeServiceKey: ServiceKey[SolutionEvent] = ServiceKey[SolutionEvent](solution.id)
    var new_final_solution = final_solution
    Behaviors.receive { (context, message) =>
      message match {
        case ListingResponse(NodeServiceKey.Listing(listings), from) =>
          val xs: Set[ActorRef[SolutionEvent]] = listings
          xs foreach { replyTo =>
            sendSolution(solution, replyTo, from)
          }
        case ReceiveOptimalSolution(optimal_solution) =>
          if (optimal_solution == null) {
            parent_ref ! SendOptimalSolution(null)
            //TODO: should break here
            Behaviors.stopped
          }
          new_final_solution = final_solution.addSolution(optimal_solution)
      }
      receive(new_final_solution)
    }
  }
}

object SolutionNode {
  sealed trait SolutionEvent//Scalas enum
  final case class ReceiveOptimalSolution(solution: Solution) extends SolutionEvent
  final case class SendSolution(color_mapping: Map[Int, String]) extends SolutionEvent
  final case class ListingResponse(listing: Receptionist.Listing, from: Int) extends SolutionEvent

  def apply(
             solution: Solution,
             mapping: Mapping,
             parent_ref: ActorRef[Node.Event]
           ): Behavior[SolutionEvent] = Behaviors.setup { context =>
    val node = new SolutionNode(solution, mapping, parent_ref, context.self)
    node.requestChildRef(context)
    node.receive(solution)
  }

}