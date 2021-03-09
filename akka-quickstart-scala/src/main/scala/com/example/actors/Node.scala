package com.example.actors

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

  def apply(node: TreeNode): Behavior[Event] = Behaviors.receive { (context, message) =>
    message match {
      case PrintGraph() =>
        context.log.info(
          "Tree Node {}, has graph nodes: {}, parent: {}, children: {}, path: {}",
          node.id,
          node.graph_variables,
          if (node.parent == null)  0  else node.parent.id,
          node.tree_childeren,
          context.self.path,
        )
        Behaviors.same

      //Solve COP for this subtree here (using something like choco solver)
      case Initialize(parent_color_mapping: Map[Int, String]) =>
        val new_node : TreeNode = node.updateNodes(parent_color_mapping)
        val solution = this.initializeNodes(node)
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
