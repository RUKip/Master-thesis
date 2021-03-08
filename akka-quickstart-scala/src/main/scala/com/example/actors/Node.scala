package com.example.actors

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.cluster.ClusterEvent.MemberEvent
import com.example.TreeNode

//This node should be representing a node in the Hypertree decomposition (else could not be solved nicely in parallel)
class Node() {

}

object Node {
  sealed trait Event //Scalas enum
  final case class PrintGraph() extends Event
  final case class Initialize(parent_color: String) extends Event
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
        //Solve COP for this subtree here (using something like choco solver?)

        //Wait for messages from children (depends on what implementation of solver, but for example see hybrid-backtracking paper could be message good/no-good)


        Behaviors.same

      case Initialize(parent_color: String)
        solve(GraphNode node, HashMap<Integer, GraphNode> mapping)
        Solver::solve(node)
        node.graph_variables.foreach()
        Behaviors.same


      case BackTrack() =>
        Behaviors.same

    }
  }

  def initializeNodes(treeNode: TreeNode): Unit ={
    treeNode.graph_variables.
  }
}
