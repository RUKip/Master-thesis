package com.example.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import com.example.actors.SolutionNode.{SendSolution, SolutionEvent}
import com.example.{CborSerializable, Mapping, Solution}
import com.fasterxml.jackson.databind.annotation.JsonDeserialize

class SolutionNode(val solution: Solution, val mapping: Mapping, val tree_node_children_ids: List[Int], val parent_node: ActorRef[NodeSearch.Event], val context: ActorContext[SolutionEvent]) {

  def sendSolution(solution: Solution, actorRef: ActorRef[Node.Event], node_id: Int): Unit = {
    context.log.info("Trying to send solution " + solution.id + " , to actor " + actorRef + "  , with id: " + node_id)
    actorRef ! Node.ReceiveSolution(mapping.getSpecificMapping(solution, node_id), context.self)
  }

  def receive(final_solution: Solution, index: Int): Behavior[SolutionEvent] = {
    var new_final_solution = final_solution
    if (index == tree_node_children_ids.size) {
      context.log.info("Done aggregating, sending optimal solution {}", final_solution.bareColorMapping())
      parent_node ! NodeSearch.SendOptimalSolution(final_solution.bareColorMapping())
      Behaviors.stopped
    } else {
      Behaviors.receive { (context, message) =>
        message match {
          case SendSolution(optimal_solution, score) =>
            context.log.info("Received a solution: {} {}", optimal_solution, score)
            if (optimal_solution.isEmpty) {
              parent_node ! NodeSearch.SendOptimalSolution(null)
              Behaviors.stopped
            } else {
              new_final_solution = final_solution.aggregateSolution(optimal_solution, score)
              receive(new_final_solution, index + 1)
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
  trait SolutionEvent extends CborSerializable//Scalas enum
  final case class SendSolution(@JsonDeserialize(keyAs = classOf[Int]) color_mapping: Map[Int, String], score: Int) extends SolutionEvent

  def apply(
             solution: Solution,
             mapping: Mapping,
             tree_node_children_ids: List[Int],
             parent_ref: ActorRef[NodeSearch.Event],
             child_refs: Map[Int, ActorRef[Node.Event]]
           ): Behavior[SolutionEvent] = Behaviors.setup { context =>
    val node = new SolutionNode(solution, mapping, tree_node_children_ids, parent_ref, context)
    child_refs.foreach{ case (key: Int, child_ref: ActorRef[Node.Event]) =>
      node.sendSolution(solution, child_ref, key)
    }
    node.receive(solution, 0)
  }

}