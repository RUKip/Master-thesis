package com.example.actors

import akka.actor.typed.Behavior
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.Behaviors
import com.example.{GraphNode, TreeNode}
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

  var NodeServiceKey: ServiceKey[Event] = _
  var tree_node: TreeNode = _

  def apply(node: TreeNode): Behavior[Event] = Behaviors.setup { context =>
    NodeServiceKey = ServiceKey[Event](node.id.toString)
    context.system.receptionist ! Receptionist.Register(NodeServiceKey, context.self)

    Behaviors.receive { (context, message) =>
      tree_node = node
      message match {
        //This is a testing behaviour
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
          val solutions = this.initializeNodes(tree_node)
          context.log.info("Solution: {}", solutions)
//          solutions.map(solution => {
//          })
          Behaviors.same

        //Wait for messages from children (depends on what implementation of solver, but for example see hybrid-backtracking paper could be message good/no-good)
        case BackTrack() =>
          Behaviors.same

      }
    }
  }

  def initializeNodes(node: TreeNode): List[Map[Int, String]] = {
    val java_mapping: Map[Integer, GraphNode] = node.full_graph_mapping map {case (key, value) => (key:java.lang.Integer, value)}
    val solutions: List[Map[Int, String]]  = Solver.solve(node.getGraphNodes.asJava, java_mapping.asJava)
      .asScala
      .toList
      .map(internal_map => internal_map.asScala.toMap map {case (key, value) => (key.toInt, value) })
    solutions
  }
}
