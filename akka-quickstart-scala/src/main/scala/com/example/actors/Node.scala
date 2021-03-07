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
  final case class PrintGraph()
  private final case class MemberChange(event: MemberEvent) extends Event

  def apply(node: TreeNode): Behavior[PrintGraph] = Behaviors.receive { (context, message) =>
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
  }
}
