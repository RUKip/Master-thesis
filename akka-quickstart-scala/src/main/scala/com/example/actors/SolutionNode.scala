package com.example.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import com.example.actors.SolutionNode.{ListingResponse, ReceiveOptimalSolution, SolutionEvent}
import com.example.{Mapping, Solution}

class SolutionNode(val solution: Solution, val mapping: Mapping, val tree_node_childeren_ids: List[Int], val parent_node: ActorRef[NodeSearch.Event], val parent_solution_node: ActorRef[SolutionEvent]) {

  def requestChildRef(context: ActorContext[SolutionEvent]): Unit = {
    //Defines what message is responded after the actor is requested from the receptionist
    val listingAdapter: ActorRef[Receptionist.Listing] =
      context.messageAdapter { listing => {
        val from: Int = listing.getKey.id.toInt
        ListingResponse(listing, from)
      }
    }

    context.log.debug("Sending request to find tree childeren: " + solution.parent.tree_childeren)
    solution.parent.tree_childeren.foreach( node =>
      context.system.receptionist ! Receptionist.Find(ServiceKey[Node.Event](node.toString), listingAdapter)
    )
  }

  def sendSolution(solution: Solution, actorRef: ActorRef[Node.Event], node_id: Int): Unit ={
    actorRef ! Node.SendSolution(mapping.getSpecificMapping(solution, node_id))
  }

  def receive(final_solution: Solution, index: Int): Behavior[SolutionEvent] = {
    val NodeServiceKey: ServiceKey[Node.Event] = ServiceKey[Node.Event](solution.id)
    var new_final_solution = final_solution
    if (index == tree_node_childeren_ids.size) {
      parent_node ! NodeSearch.SendOptimalSolution(final_solution.color_mapping)
      Behaviors.stopped
    } else {
      Behaviors.receive { (context, message) =>
        message match {
          case ListingResponse(NodeServiceKey.Listing(listings), from) =>
            context.log.debug("Got address from receptionist for: " + from)
            val xs: Set[ActorRef[Node.Event]] = listings
            xs foreach { replyTo =>
              sendSolution(solution, replyTo, from)
            }
            receive(new_final_solution, index)
          case ReceiveOptimalSolution(optimal_solution) =>
            if (optimal_solution == null) {
              parent_node ! NodeSearch.SendOptimalSolution(null)
              Behaviors.stopped
            } else {
              new_final_solution = final_solution.addSolution(optimal_solution)
              receive(new_final_solution, index)
            }
          case _ =>
            //TODO: It gets here...  message = ListingResponse(Listing(ServiceKey[com.example.actors.Node$Event](2),Set(),Set(),true),2)
            context.log.error("Couldn't match m essage!: " + message)
            receive(new_final_solution, index)
        }
      }
    }
  }
}

object SolutionNode {
  trait SolutionEvent//Scalas enum
  final case class ReceiveOptimalSolution(solution: Solution) extends SolutionEvent
  final case class SendSolution(color_mapping: Map[Int, String]) extends SolutionEvent
  final case class ListingResponse(listing: Receptionist.Listing, from: Int) extends SolutionEvent

  def apply(
             solution: Solution,
             mapping: Mapping,
             tree_node_childeren_ids: List[Int],
             parent_ref: ActorRef[NodeSearch.Event]
           ): Behavior[SolutionEvent] = Behaviors.setup { context =>
    val node = new SolutionNode(solution, mapping, tree_node_childeren_ids, parent_ref, context.self)
    node.requestChildRef(context)
    node.receive(solution, 0)
  }

}