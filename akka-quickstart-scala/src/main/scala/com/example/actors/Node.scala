package com.example.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import com.example.actors.Node._
import com.example.actors.SolutionNode.SolutionEvent
import com.example.TreeNode

//This node should be representing a node in the Hypertree decomposition (else could not be solved nicely in parallel)
class Node(child_refs: Map[Int, ActorRef[Node.Event]]) {

  def receive(tree_node: TreeNode, solution_id: Int): Behavior[Event] = {
    Behaviors.receive { (context, message) =>
      message match {
        //This is a testing behaviour
        case PrintGraph() =>
          context.log.info(
            "Tree Node {}, has graph nodes: {}, parent: {}, children: {}, path:  {}",
            tree_node.id,
            tree_node.graph_variables,
            tree_node.parent,
            tree_node.tree_children,
            context.self.path,
          )
          Behaviors.same
        case ReceiveSolution(parent_color_mapping: Map[Int, String], solution_node: ActorRef[SolutionEvent]) =>
          context.log.info("Node received solution: " + parent_color_mapping.toString() + " from solution node: " + solution_node.path.toString)
          val new_node : TreeNode = tree_node.updateNodes(parent_color_mapping)
          context.spawn(
            NodeSearch(new_node, child_refs, context.self, solution_node),
            solution_id.toString
          )
          receive(new_node, solution_id+1)
        case Terminate() =>
          context.log.info("Terminating..")
          child_refs.values foreach { replyTo =>
            replyTo ! Terminate()
          }
          Behaviors.stopped
        case _ =>
          context.log.error("Unexpected message: " + message)
          Behaviors.stopped
      }
    }
  }
}

object Node {
  sealed trait Event //Scalas enum
  final case class PrintGraph() extends Event
  final case class ReceiveSolution(parent_color_mapping: Map[Int, String], from: ActorRef[SolutionEvent]) extends Event
  final case class Terminate() extends Event

  def apply(tree_node: TreeNode, child_refs: Map[Int, ActorRef[Node.Event]]): Behavior[Event] = Behaviors.setup { context =>
    val node = new Node(child_refs)

    context.log.info("Node " + tree_node.id + " setup, starting to receive")
    node.receive(tree_node, 0)
  }
}
