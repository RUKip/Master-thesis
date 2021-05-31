package com.example.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import com.example.actors.SolutionNode.{ReceiveOptimalSolution, SolutionEvent}
import com.example.{Mapping, Solution}

class SolutionNode(val solution: Solution, val mapping: Mapping, val tree_node_childeren_ids: List[Int], val parent_node: ActorRef[NodeSearch.Event], val parent_solution_node: ActorRef[SolutionEvent]) {

  def sendSolution(solution: Solution, actorRef: ActorRef[Node.Event], node_id: Int, context: ActorContext[SolutionEvent]): Unit = {
    context.log.info("Trying to send solution " + solution.id + " , to actor " + actorRef + "  , with id: " + node_id)
    actorRef ! Node.ReceiveSolution(mapping.getSpecificMapping(solution, node_id), context.self)
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
            context.log.error("Unexpected message: " + message)
            Behaviors.stopped
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
      node.sendSolution(solution, child_ref, key, context)
    }
    node.receive(solution, 0)
  }

}