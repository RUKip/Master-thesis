package com.example.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import com.example.actors.SolutionNode.{ReceiveOptimalSolution, SolutionEvent}
import com.example.{Mapping, Solution}

class SolutionNode(val solution: Solution, val mapping: Mapping, val tree_node_childeren_ids: List[Int], val parent_node: ActorRef[NodeSearch.Event], val parent_solution_node: ActorRef[SolutionEvent]) {

  def sendSolution(solution: Solution, actorRef: ActorRef[Node.Event], node_id: Int): Unit = {
    actorRef ! Node.SendSolution(mapping.getSpecificMapping(solution, node_id))
  }

  def receive(final_solution: Solution, index: Int): Behavior[SolutionEvent] = {
    var new_final_solution = final_solution
    if (index == tree_node_childeren_ids.size) {
      parent_node ! NodeSearch.SendOptimalSolution(final_solution.color_mapping)
      Behaviors.stopped
    } else {
      Behaviors.receive { (context, message) =>
        message match {
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
            context.log.error("Couldn't match message!: " + message)
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

  def apply(
             solution: Solution,
             mapping: Mapping,
             tree_node_childeren_ids: List[Int],
             parent_ref: ActorRef[NodeSearch.Event],
             child_refs: Map[Int, ActorRef[Node.Event]]
           ): Behavior[SolutionEvent] = Behaviors.setup { context =>
    val node = new SolutionNode(solution, mapping, tree_node_childeren_ids, parent_ref, context.self)
    child_refs.foreach{ case (key: Int, child_ref: ActorRef[Node.Event]) =>
      node.sendSolution(solution, child_ref, key)
    }
    node.receive(solution, 0)
  }

}