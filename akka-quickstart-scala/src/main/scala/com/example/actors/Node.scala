package com.example.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import com.example.TreeNode

//This node should be representing a node in the Hypertree decomposition (else could not be solved nicely in parallel)
class Node() {

}

object Node {
  final case class PrintGraph()

  def apply(node: TreeNode): Behavior[PrintGraph] = Behaviors.receive { (context, message) =>
    context.log.info(
      "Node {}, has color: {}, children: {}",
      node.id,
      node.color,
      node.children.map(
        (child: TreeNode) => child.id
      )
    )

    node.children.foreach( child => {
      val child_node = context.spawn(Node(child), child.id.toString)
      child_node ! PrintGraph()
    })
    //Solve COP for this subtree here (using something like choco solver?)

    //Wait for messages from children (depends on what implementation of solver, but for example see hybrid-backtracking paper could be message good/no-good)


    Behaviors.same
  }
}
