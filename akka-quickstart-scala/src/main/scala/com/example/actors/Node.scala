package com.example.actors

import akka.actor.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import com.example.TreeNode
import com.example.solver.Solver

import scala.jdk.CollectionConverters._

//This node should be representing a node in the Hypertree decomposition (else could not be solved nicely in parallel)
class Node() {

}

object Node {
  sealed trait Event //Scalas enum
  final case class PrintGraph() extends Event
  final case class Initialize(parent_color_mapping: Map[Int, String]) extends Event
  final case class BackTrack() extends Event

  var tree_node: TreeNode = _

  def apply(node: TreeNode): Behavior[Event] = Behaviors.receive { (context, message) =>
    tree_node = node
    message match {
      case PrintGraph() =>
        context.log.info(
          "Tree Node {}, has graph nodes: {}, parent: {}, children: {}, path: {}",
          tree_node.id,
          tree_node.graph_variables,
          if (tree_node.parent == null)  0  else tree_node.parent.id,
          tree_node.tree_childeren,
          context.self.path,
        )
        Behaviors.same

      //Solve COP for this subtree here (using something like choco solver)
      case Initialize(parent_color_mapping: Map[Int, String]) =>
        val new_node : TreeNode = tree_node.updateNodes(parent_color_mapping)
        tree_node = new_node
        val solution = this.initializeNodes(tree_node)
        context.log.info("Solution: {}", solution)
        Behaviors.same

      //Wait for messages from children (depends on what implementation of solver, but for example see hybrid-backtracking paper could be message good/no-good)
      case BackTrack() =>
        Behaviors.same

    }
  }

  def initializeNodes(node: TreeNode): String ={
    val java_mapping = node.full_graph_mapping map {case (key, value) => (key:java.lang.Integer, value)};
    val solution = Solver.solve(node.getGraphNodes.asJava, java_mapping.asJava)
    solution
  }
}
