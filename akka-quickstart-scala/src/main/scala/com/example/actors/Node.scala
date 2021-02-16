package com.example.actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import com.example.TreeNode

class Node() {

//  final case class SolveTree(whom: String, replyTo: ActorRef)


//  def apply(): Behavior[SolveTree] = Behaviors.receive { (context, message) =>
//    context.log.info("Hello {}!", message.whom)
//    message.replyTo ! Greeted(message.whom, context.self)
//    Behaviors.same
//  }


}

object Node {
  final case class PrintGraph()

  def apply(node: TreeNode): Behavior[PrintGraph] = Behaviors.receive { (context, message) =>
    context.log.info(
      "Node {}, has color: {}, childeren: {}",
      node.id,
      node.color,
      node.childeren.map(
        (child: TreeNode) => child.id
      )
    )

    node.childeren.foreach( child => {
      val child_node = context.spawn(Node(child), child.id.toString)
      child_node ! PrintGraph()
    }
    )
    Behaviors.same
  }
}
