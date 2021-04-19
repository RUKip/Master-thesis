package com.example.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import com.example.{Mapping, Solution}

class SolutionNode {

}

object SolutionNode {
  sealed trait SolutionEvent //Scalas enum
  final case class ReceiveOptimalSolution(solution: Solution) extends SolutionEvent
  final case class ListingResponse(listing: Receptionist.Listing, from: Int) extends SolutionEvent
  final case class SendOptimalSolution(solution: Solution) extends SolutionEvent


  def apply(
             solution: Solution,
             mapping: Mapping,
             parent_ref: ActorRef[SolutionEvent]
           ): Behavior[SolutionEvent] = Behaviors.setup { context =>
    requestChildRef(solution, context)
    var final_solution = Solution()
    receive(parent_ref, solution, mapping, final_solution)
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

  def sendSolution(actorRef: ActorRef[SolutionEvent], node_id: Int): Unit ={
    actorRef ! mapping.getSpecificMapping(solution, node_id)
  }

  def receive(parent_ref: ActorRef[SolutionEvent], solution: Solution, mapping: Mapping, final_solution: Solution): Behavior[SolutionEvent] = {
    val NodeServiceKey: ServiceKey[SolutionEvent] = ServiceKey[SolutionEvent](solution.id)
    Behaviors.receive { (context, message) =>
      message match {
        case ListingResponse(NodeServiceKey.Listing(listings), from) =>
          val xs: Set[ActorRef[SolutionEvent]] = listings
          xs foreach { replyTo =>
            sendSolution(replyTo, from)
          }
          Behaviors.same
        case ReceiveOptimalSolution(solution) =>
          if (solution == null) {
            parent_ref ! SendOptimalSolution(null)
            //TODO: should break here
            Behaviors.empty
          }
          //TODO: Add value to final_solution here
          receive(parent_ref, solution, mapping, final_solution)

      }
    }
  }
}