package com.example.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.cluster.ClusterEvent.MemberEvent
import com.example.GraphNode

//This node should be representing a node in the Hypertree decomposition (else could not be solved nicely in parallel)
class Node() {

}

object Node {
  sealed trait Event //Scalas enum
  final case class PrintGraph()
  private final case class MemberChange(event: MemberEvent) extends Event

  def apply(node: GraphNode, mapping: Map[Int, GraphNode]): Behavior[PrintGraph] = Behaviors.receive { (context, message) =>
    context.log.info(
      "Node {}, has color: {}, connected: {}, path: {}",
      node.id,
      node.color,
      node.connected_ids,
      context.self.path,

    )
    //Solve COP for this subtree here (using something like choco solver?)

    //Wait for messages from children (depends on what implementation of solver, but for example see hybrid-backtracking paper could be message good/no-good)


    Behaviors.same
  }
}
